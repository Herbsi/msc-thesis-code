## Packages --------------------------------------------------------------------
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(parallel)
library(purrr, warn.conflicts = FALSE)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")


## Simulation ------------------------------------------------------------------

r_model <- function(n, model_name, x) {
  switch(model_name,
    "Linear" = 2 * x + rnorm(n),
    "Normal" = rnorm(n, x, 1),
    "D" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) + 10 * (x >= 5),
    "P" = rpois(n, pmin(pmax(x, 1), 6)),
    "NI" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 * (x > 7),
    "S" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)),
    "AR(1)" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)),
    "AR(2)" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)),
    "S1" = runif(n, 1 + x / 10, 2 * (1 + x / 10)),
    "S1_2" = rbeta(n, shape = 1 + x / 10, shape2 = 2),
    "S1_3" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6))
  )
}


generate_data <- function(n_train, n_valid, n_test, model_name) {
  n <- n_train + n_valid + n_test
  switch(model_name,
    "AR(1)" = {
      X <- numeric(n)
      steps <- runif(n - 1, -1, 1)
      X[1] <- runif(1, 0, 10)
      for (i in 2:(n_train+n_valid)) {
        X[i] <- 0.95 * X[i-1] + steps[i - 1]
      }
      for (i in (n_train+n_valid+(1:n_test))) {
        X[i] <- 0.95 * X[n_train+n_valid] + steps[i - 1]
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      Y <- r_model(n, "S", X)
      tibble(X = X, Y = Y)
    },
    "AR(2)" = {
      X <- numeric(n)
      steps <- runif(n - 1, -1, 1)
      X[c(1, 2)] <- runif(2, 0, 10)
      for (i in 3:(n_train+n_valid)) {
        X[i] <- 0.95 * X[i-1] - 0.94 * X[i-2] + steps[i - 2]
      }
      for (i in (n_train+n_valid+(1:n_test))) {
        X[i] <- 0.95 * X[n_train+n_valid] - 0.94 * X[n_train+n_valid-1] + steps[i - 2]
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      Y <- r_model(n, "S", X)
      tibble(X = X, Y = Y)
    },
    tibble(X = runif(n, 0, 10), Y = r_model(n, model_name, X))
  )
}


make_simulation_run <- function(runs, alpha_sig, results_dir) {
  method_list <- list(QR = dcp_qr,
    "QR*" = dcp_qr_opt,
    DR = dcp_dr,
    IDR = dcp_idr,
    "IDR*" = dcp_idr_opt,
    CP_OLS = dcp_cp_ols,
    CP_LOC = dcp_cp_loc)


  noise <- function(n) {
    runif(n, -1e-6, 1e-6)
  }

  
  function(n, model_name, method_name) {
    writeLines(str_c(n, model_name, method_name, sep = " "))
    start <- Sys.time()
    
    ## Setup
    set.seed(42 + n) # Ensure we generate the same data for every `n'
    method <- method_list[[method_name]]

    n_train <- n / 2
    n_valid <- n / 2
    n_test <- 4096

    ## Function to perform a single run.
    ## This should be fairly fast, as it gets called /a lot/.
    single_run <- function() {
      data_tibble <- as.data.table(generate_data(n_train, n_valid, n_test, model_name))
      
      data_train <- data_tibble[1:n_train]
      data_valid <- data_tibble[(n_train+1):(n_train+n_valid)]
      data_test <- data_tibble[(n_train+n_valid+1):(n_train+n_valid+n_test)]

      ## Add noise to training data
      data_train[, X := X + noise(n_train)]
      data_train[, Y := Y + noise(n_train)]
      
      ## `data.table' with 3 columns:
      ## - X = { Xₙ₊₁⁽ʲ⁾ : j = 1 … n_test } 
      ## - conditional_coverage { 1{ Yₙ₊₁⁽ʲ⁾ ∈ C(Xₙ₊₁⁽ʲ⁾) : j = 1 … n_test })
      ## - conditional_leng { |C(Xₙ₊₁⁽ʲ⁾)| : j = 1 … n_test } a list of estimates for C(Xₙ₊₁)
      method(Y ~ X, data_train, data_valid, data_test, alpha_sig)
    }

    num_cores <- detectCores() - 1

    ## Perform the different runs in parallel.
    simulation_result <- mclapply(1:runs, \(x) single_run(), mc.cores = num_cores) |>
      list_rbind()

    elapsed <- difftime(Sys.time(), start, units = "secs")
    writeLines(str_c("Time:", elapsed, sep = " "))


    ## Save raw results to file
    filename <- file.path(results_dir, temp_result_filename(n, model_name, method_name))
    save(simulation_result, file = filename)


    ## Return result
    simulation_result
  } 
}


summarise_simulation <- function(simulation_result, X_grid, X_breaks) {
  ## Summarise conditional coverage by fitting a logistic regression
  cc_glm <- suppressWarnings(glm(conditional_coverage ~ X, data = simulation_result, family = binomial(link = "logit")))
  conditional_coverage_tibble <- tibble(
    X = X_grid,
   conditional_coverage = predict(cc_glm, newdata = data.frame(X = X_grid), type = "response"))

  ## Summarise conditional length by binning
  conditional_leng_binned <- simulation_result |>
    mutate(bin = cut(X, breaks = X_breaks, include.lowest = TRUE, ordered_result = TRUE)) |>
    group_by(bin) |>
    summarise(conditional_leng = mean(conditional_leng, na.rm = TRUE))

  ## End result
  simulation_result |>
    summarise(
      coverage = mean(conditional_coverage), # Unconditional coverage is just the mean coverage
      leng = mean(conditional_leng, na.rm = TRUE), # Unconditional length also
      ## ‘Summarise’ more-complex objects by nesting them into a list.
      conditional_coverage = list(conditional_coverage_tibble),
      conditional_leng = list(conditional_leng_binned),
      cc_glm = list(cc_glm))
}


run_experiment <- function(results_dir,
                           model_name = c("D", "P", "NI", "S",
                             "AR(1)", "AR(2)",
                             "S1", "S1_2", "S1_3"),
                           method_name = c("QR", "QR*",
                             "DR",
                             "IDR", "IDR*",
                             "CP_OLS", "CP_LOC"),
                           runs = 500,
                           alpha_sig = 0.1,
                           n = 2^(7:16)) {
  results_dir <- file.path("results",
    results_dir,
    format(Sys.time(), "%Y%m%d%H%M%S"))
  dir.create(results_dir, recursive = TRUE)
  
  run_simulation <- make_simulation_run(runs, alpha_sig, results_dir)
  results_tibble <- crossing(
    ## Create tibble with all combinations of `n', `model' and `method'
    tibble(n = n),
    tibble(model_name = model_name),
    tibble(method_name = method_name)) |>
    mutate(compute = pmap(across(everything()), run_simulation))

  ## Auxiliary values for summary
  X_min <- map_dbl(results_tibble$compute, \(sim_res) min(sim_res$X)) |> min()
  X_max <- map_dbl(results_tibble$compute, \(sim_res) max(sim_res$X)) |> max()
  X_grid <- seq(X_min, X_max, length.out = 100)
  X_breaks <- seq(X_min, X_max, length.out = 21)

  num_cores <- detectCores() - 1

  results_tibble <- results_tibble |>
    mutate(compute = mclapply(compute,
      ## By design, all the X values are, more or less, in [0, 10].
      ## To make plotting more convenient, we bin all simulation results the same, between the overall minimal and maximal test values observed.
      \(simulation_result) summarise_simulation(simulation_result, X_grid, X_breaks),
      mc.cores = num_cores),
      .keep = "unused") |>
    unnest(compute)

  save(results_tibble, file = file.path(results_dir, final_result_filename()))

  results_tibble
}


## Summaries --------------------------------------------------------------------

conditional_calc <- function(column, fn) {
  function(results_tibble, new_name, alpha_sig = 0.1) {
    results_tibble |>
      mutate({{ new_name }} := map2_dbl({{ column }}, alpha_sig, fn))
  }
}

conditional_coverage_mse <- conditional_calc(conditional_coverage,
  \(df, alpha_sig) sqrt(mean((df$conditional_coverage - (1 - alpha_sig))^2)))

conditional_coverage_mae <- conditional_calc(conditional_coverage,
  \(df, alpha_sig) mean(abs(df$conditional_coverage - (1 - alpha_sig))^2))

conditional_coverage_sd <- conditional_calc(conditional_coverage,
  \(df, alpha_sig) sd(df$conditional_coverage))

conditional_leng_sd <- conditional_calc(conditional_leng,
  \(df, alpha_sig) sd(df$conditional_leng))


## Helpers ----------------------------------------------------------------------

temp_result_filename <- function(n, model_name, method_name) {
  str_c(str_c(n, model_name, method_name, sep = "_"), ".RData")
}


final_result_filename <- function() {
  str_c("results_tibble", format(Sys.time(), "%H%M%S"), ".RData")
}


merge_results <- function(results_dir,
                          ts,
                          model_name = c("D", "P", "NI", "S", "AR(1)", "AR(2)", "S1", "S1_2", "S1_3"),
                          method_name = c("DR", "QR", "QR*", "IDR", "IDR*", "CP_OLS", "CP_LOC"),
                          n = 2^(7:16)) {
  results_dir <- file.path("results", results_dir, ts)
  
  df <- crossing(tibble(n = n),
    tibble(model_name = model_name),
    tibble(method_name = method_name))
  
  files <- file.path(results_dir, temp_result_filename(df$n, df$model_name, df$method_name))
  df2 <- tibble()
  walk(files, \(f) {
    print(str_c("Loading ", f))
    load(force(f), verbose = TRUE)
    df2 <<- bind_rows(df2, tribble(~coverage, ~leng, ~conditional_coverage,
      ~conditional_leng, simulation_result$coverage, simulation_result$leng,
      simulation_result$conditional_coverage,
      simulation_result$conditional_leng))
  }
  )
  
  results_tibble <- bind_cols(df, df2) |>
    mutate(conditional_coverage = map(conditional_coverage, ~ .x[[1]]),
      conditional_leng = map(conditional_leng, ~ .x[[1]]))
  save(results_tibble,
    file = file.path(results_dir, final_result_filename()))
  
  results_tibble
}


make_values_filter <- function(values) {
  function(data, column = model_name, .by = NULL, .preserve = FALSE) {
    data |>
      filter({{ column }} %in% force(values), .by = all_of(.by), .preserve = .preserve)
  }
}

filter_independent <- make_values_filter(c("D", "P", "NI", "S"))
filter_dependent <- make_values_filter(c("AR(1)", "AR(2)"))
filter_thm4 <- make_values_filter(c("S1", "S1_2", "S1_3"))
