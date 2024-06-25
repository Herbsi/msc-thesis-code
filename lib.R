## Packages --------------------------------------------------------------------
library(dplyr)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")


## Simulation ------------------------------------------------------------------

generate_data <- function(n, model_name) {
  switch(model_name,
    "D" = tibble(
      X = runif(n, 0, 10),
      Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6)) + 10 * (X >= 5)) ,
    "P" = tibble(
      X = runif(n, 0, 10),
      Y = rpois(n, pmin(pmax(X, 1), 6))),
    "NI" = tibble(
      X = runif(n, 0, 10),
      Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6)) - 2 *(X > 7)),
    "S" = tibble(
      X = runif(n, 0, 10),
      Y = rgamma(n, shape = sqrt(X), scale = pmin(pmax(X, 1), 6))),
    "AR(1)" = {
      X <- rep(0, times = n)
      X[1] <- runif(1, 0, 10)
      for (i in 2:n) {
        X[i] <- 0.95 * X[i-1] + runif(1, -1, 1)
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      Y <- 2 * X + rnorm(n, 0, 2) # Simple linear relationship
      tibble(X = X, Y = Y) 
    },
    "AR(2)" = {
      X <- rep(0, times = n)
      X[c(1, 2)] <- runif(2, 0, 10)
      for (i in 3:n) {
        X[i] <- 0.95 * X[i-1] - 0.94 * X[i-2] + runif(1, -1, 1)
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      Y <- 2 * X + rnorm(n, 0, 2)
      tibble(X = X, Y = Y)
    },
    "S1" = { ## 0 < C1, C2 < ∞
      tibble(X = runif(n, 0, 10),
        Y = runif(n, (1 + X / 10), 2 * (1 + X / 10)),
        C1 = 1 / 2, C2 = 4)
    },
    "S1_2" = { ## C1 = 0, 0 < C2 < ∞
      tibble(X = runif(n, 0, 10),
        Y = rbeta(n, shape1 = 2 + X / 10, shape2 = 2 + 2 * X / 10), C1 = 0,
        C2 = 1)
    },
    "S1_3" = { ## C1 = 0, C2 = ∞
      tibble(X = runif(n, 0, 10), Y = rnorm(n, mean = X), C1 = 0, C2 = Inf) 
    }
  )
}


make_simulation <- function(runs, alpha_sig, results_dir) {
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
    n_total <- n_train + n_valid + n_test
    
    ind_train <- sort(sample(1:n_total, size = n_train))
    ind_valid <- sort(sample((1:n_total)[-ind_train], size = n_valid))
    ind_test <- (1:n_total)[-c(ind_train, ind_valid)]

    split <- function(df) {
      list(train = df[ind_train, , drop = FALSE],
        valid = df[ind_valid, , drop = FALSE],
        test = df[ind_test, , drop = FALSE]
      )
    }

    noise <- function(ind) {
      noise <- rep(0, times = n_total)
      noise[ind] <- runif(length(ind), -1e-6, 1e-6)
      noise
    }
    
    prediction_interval <- seq(0, 10, length.out = 100)
    breaks <- seq(0, 10, length.out = 21)

    simulation_result <- replicate(runs, simplify = FALSE, expr = {
      data_tibble <- generate_data(n_total, model_name) |>
        mutate( # Add noise to training data.
          X = X + noise(ind_train),
          Y = Y + noise(ind_train),
          )
      
      with(method(Y ~ X, data_tibble, split, alpha_sig),
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
        }
      )

    filename <- file.path(results_dir,
      str_c(n, model_name, method_name, sep = "_") |> str_c(".RData"))
    ## Save results to file
    save(simulation_result, file = filename)

    ## Return result
    simulation_result
  }
}



theorem_3 <- function(runs = 500,
                      alpha_sig = 0.1,
                      n = 2^(5:16),
                      method_name = c("QR", "DR", "IDR", "CP_OLS", "CP_LOC"),
                      model_name = c("D", "P", "NI", "S", "AR(1)"),
                      subdir = format(Sys.time(), "%Y%m%d")) {
  results_dir <- file.path("results", "theorem-3", subdir)
  dir.create(results_dir)

  run_simulation <- make_simulation(runs, alpha_sig, results_dir)

  results_tibble <- crossing(
    ## Create tibble with all combinations of `n', `model' and `method'
    tibble(n = n),
    tibble(model_name = model_name),
    tibble(method_name = method_name)) |>
    mutate(compute = pmap(across(everything()), run_simulation)) |>
    unnest(compute)

  save(results_tibble, file = file.path(results_dir, "results_tibble.RData"))

  results_tibble
}


merge_results <- function(n, ts) {
  df <- crossing(
    tibble(n = n),
    tibble(model_name = c("D", "P", "NI", "S", "AR")),
    tibble(method_name = c("QR", "DR", "IDR", "CP_OLS", "CP_LOC"))
  )
  files <- str_c("results/theorem-3-euler/", ts, "/",
    str_c(df$n, df$model_name, df$method_name, sep = "_")) |>
    str_c(".RData")

  df2 <- tibble()
  walk(files, \(f) {
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
    file = str_c("results/theorem-3-euler/results_tibble",
      format(Sys.time(), "%Y%m%d%H%M%S"), ".RData"))
  results_tibble
}
