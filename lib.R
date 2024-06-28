## Packages --------------------------------------------------------------------
library(dplyr)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")


## Simulation ------------------------------------------------------------------

generate_data <- function(n_train, n_valid, n_test, model_name) {
  generate_dataset <- switch(model_name,
    "D" = \(n, ...) {
      tibble(
        X = runif(n, 0, 10),
        Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6)) + 10 * (X >= 5)
      )
    },
    "P" = \(n, ...) {
      tibble(
        X = runif(n, 0, 10),
        Y = rpois(n, pmin(pmax(X, 1), 6))
      )
    },
    "NI" = \(n, ...) {
      tibble(
        X = runif(n, 0, 10),
        Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6)) - 2 *(X > 7)
      )
    },
    "S" = \(n, ...) {
      tibble(
        X = runif(n, 0, 10),
        Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6))
      )
    },
    "AR(1)" = {
      \(n, X1, ...) {
        X <- rep(0, times = n)
        X[1] <- X1
        for (i in 2:n) {
          X[i] <- 0.95 * X[i-1] + runif(1, -1, 1)
        }
        X <- X + 5
        X <- pmin(pmax(X, 0), 10)
        Y <- 2 * X + rnorm(n, 0, 2) # Simple linear relationship
        tibble(X = X, Y = Y)
      }
    } ,
    
    "AR(2)" = \(n, X1, X2, ...) {
      X <- rep(0, times = n)
      X[c(1, 2)] <- c(X1, X2)
      for (i in 3:n) {
        X[i] <- 0.95 * X[i-1] - 0.94 * X[i-2] + runif(1, -1, 1)
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      Y <- 2 * X + rnorm(n, 0, 2)
      tibble(X = X, Y = Y)
    },
    "S1" = \(n, ...) { ## 0 < C1, C2 < ∞, ie correct assumptions
      tibble(
        X = runif(n, 0, 10),
        Y = runif(n, (1 + X / 10), 2 * (1 + X / 10)),
        C1 = 1 / 2, C2 = 4)
    },
    "S1_2" = \(n) { ## C1 = 0, 0 < C2 < ∞
      tibble(
        X = runif(n, 0, 10),
        Y = rbeta(n, shape1 = 2 + X / 10, shape2 = 2 + 2 * X / 10),
        C1 = 0,
        C2 = 1)
    },
    "S1_3" = \(n) { ## C1 = 0, C2 = ∞
      tibble(X = runif(n, 0, 10), Y = rnorm(n, mean = X), C1 = 0, C2 = Inf) 
    })
  ## Fix starting values of AR models
  X1 <- runif(1, 0, 10)
  X2 <- runif(1, 0, 10)
  data_tibbles <- lapply(c(n_train, n_valid, n_test), \(n) generate_dataset(n, X1, X2))
  names(data_tibbles) <- c("data_train", "data_valid", "data_test")
  data_tibbles
}


make_simulation_run <- function(runs, alpha_sig, results_dir) {
  method_list <- list(QR = dcp_qr,
    "QR*" = dcp_qr_opt,
    DR = dcp_dr,
    IDR = dcp_idr,
    "IDR*" = dcp_idr_opt,
    CP_OLS = dcp_cp_ols,
    CP_LOC = dcp_cp_loc)
  
  function(n, model_name, method_name) {
    writeLines(str_c(n, model_name, method_name, sep = " "))
    
    ## Setup
    set.seed(42 + n) # Ensures we generate the same data for every `n'
    method <- method_list[[method_name]]

    n_train <- n / 2
    n_valid <- n / 2
    n_test <- 8192

    noise <- function(n) {
      runif(n, -1e-6, 1e-6)
    }
    
    prediction_interval <- seq(0, 10, length.out = 100)
    breaks <- seq(0, 10, length.out = 21)

    simulation_result <- replicate(runs, simplify = FALSE, expr = {
      with(generate_data(n_train, n_valid, n_test, model_name), {
        data_train$X <- data_train$X + noise(n_train)
        data_train$Y <- data_train$Y + noise(n_train)
        
        with(method(Y ~ X, data_train, data_valid, data_test, alpha_sig),
          tibble(
            coverage = coverage,
            leng = leng,
            conditional_coverage = list({
              data.frame(X = prediction_interval,
                conditional_coverage = predict_glm_from_tidy(conditional_coverage,
                  prediction_interval))
            }),
            conditional_leng = list({
              conditional_leng |>
                mutate(bin = cut(X, breaks = breaks, include.lowest = TRUE,
                  ordered_result = TRUE)) |>
                group_by(bin) |> # Mean over the bin
                summarise(conditional_leng = mean(conditional_leng, na.rm = TRUE))
            })))
      })
    }) |>
      list_rbind() |>
      summarise(
        coverage = mean(coverage),
        leng = mean(leng),
        ## Mean conditional values over data randomness
        conditional_coverage = {
          list_rbind(conditional_coverage) |>
            group_by(X) |>
            summarise(conditional_coverage = mean(conditional_coverage)) |>
            list()
        },
        conditional_leng = {
          list_rbind(conditional_leng) |>
            group_by(bin) |> 
            summarise(conditional_leng = mean(conditional_leng, na.rm = TRUE)) |>
            list()
        })

    filename <- file.path(results_dir,
      temp_result_filename(n, model_name, method_name))
    ## Save results to file
    save(simulation_result, file = filename)

    ## Return result
    simulation_result
  }
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
    mutate(compute = pmap(across(everything()), run_simulation)) |>
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
                          method_name = c("QR", "QR*", "IDR", "IDR*", "CP_OLS", "CP_LOC"),
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
      filter({{ column }} %in% force(values), .by = .by, .preserve = .preserve)
  }
}

filter_independent <- make_values_filter(c("D", "P", "NI", "S"))
filter_dependent <- make_values_filter(c("AR(1)", "AR(2)"))
filter_thm4 <- make_values_filter(c("S1", "S1_2", "S1_3"))
