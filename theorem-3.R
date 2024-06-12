## Packages
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")

current_time <- format(Sys.time(), "%Y%m%d%H%M%S")

results_dir <- file.path("results", "theorem-3", current_time)
dir.create(results_dir)

## -----------------------------------------------------------------------------
## Functions
## -----------------------------------------------------------------------------

run_simulation <- function(n, model_name, model, method_name, method) {
  writeLines(str_c(n, model_name, method_name, sep = " "))
  
  ## Setup
  set.seed(42 + n) # Ensures we generate the same data for every `n'
  
  n_train <- n / 4
  n_valid <- n / 2
  n_test <- n / 4
  
  ind_train <- 1:n_train
  ind_valid <- (n_train+1):(n_train+n_valid)
  ind_test <- (n_train+n_valid+1):n
  
  split <- function(df) {
    list(train = df[ind_train, , drop = FALSE],
      valid = df[ind_valid, , drop = FALSE],
      test = df[ind_test, , drop = FALSE]
    )
  }

  simulation_result <- replicate(runs, simplify = FALSE, expr = {
    data_tibble <- tibble(
      X = runif(n, 0, 10),
      Y = model(X)
    ) |>  
      mutate( # Add noise to training data.
        X = X + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train)),
        Y = Y + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train))
      )
    
    results_list <- method(Y ~ X, data_tibble, split, alpha_sig)

    tibble(coverage = mean(results_list$coverage),
      leng = mean(results_list$leng, na.rm = TRUE), # Just due to randomness, we sometimes end up with `-Inf', hence `NA', for `leng'
      # For simplicity, we just remove those when taking the mean over the test set.
      conditional = list(results_list$conditional_glm))
  }) |>
  list_rbind() |>
  summarise(
    coverage = mean(coverage),
    leng = mean(leng),
    conditional = list(conditional)) # We want to keep all `runs' glms at hand, so we ‘summarise’ them by nesting them into a list

  filename <- file.path(results_dir, str_c(n, model_name, method_name, sep = "_") |> str_c(".RData"))
  ## Save results to file
  save(simulation_result, file = filename)

  ## Return result
  simulation_result
}


predict_from_tidy <- function(tidy_model, x_values) {
  intercept <- tidy_model |> filter(term == "(Intercept)") |> pull(estimate)
  slope <- tidy_model |> filter(term == "X") |> pull(estimate)
  linear_predictor <- intercept + slope * x_values
  plogis(linear_predictor)
}


pred_cond_coverage <- function(results_tibble) {
  prediction_interval <- seq(0, 10, length.out = 100)

  results_tibble |>
    mutate(conditional = map(conditional, # Map the column
      \(model_list) {
        map(model_list, # Map the list in the current row
          \(tidy_model) {
            data.frame(
              X = prediction_interval,
              prediction = predict_from_tidy(tidy_model, prediction_interval))
          })
      }))
}


calc_coverage_per_X <- function(pred_tibble) {
  pred_tibble |>
    mutate(conditional = map(conditional,
      \(tibble_list) {
        bind_rows(tibble_list) |>
          group_by(X) |>
          summarise(
            conditional_coverage = mean(prediction),
            cc_std = sd(prediction)
          )
      }))
}


calc_uncond_coverage_sd <- function(pred_tibble) {
  pred_tibble |>
    mutate(coverage_sd = map_dbl(conditional, # Map the column
      \(tibble_list) {
        map_dbl(tibble_list, # Map the list inside the current cell
          \(pred_tibble) {
            sd(pred_tibble$prediction - 0.9)
          }) |>
          mean() # Mean over the data
      })
    )
}


plot_unconditional <- function(results_tibble, var) {
  results_tibble |>
    pivot_longer(cols = c(coverage, leng), names_to = "metric", values_to = "value") |>
    ggplot(aes(x = n, y = value, color = method_name, group = method_name)) +
    geom_line() +
    facet_grid(metric ~ model_name, scales = "free_y") +
    labs(
      title = "Coverage and Leng vs n for each Method and Model",
      x = "n",
      color = "Method",
    ) +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}


plot_sd <- function(pred_tibble) {
  calc_uncond_coverage_sd(pred_tibble) |>
    ggplot(aes(x = n, y = coverage_sd, color = method_name, group = method_name)) +
    geom_line() +
    facet_wrap(~ model_name) +
    labs(title = "sd(coverage) vs n for each Method and Model",
      x = "n",
      y = "sd(coverage)",
      color = "Method") +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}


plot_cond_coverage <- function(pred_tibble) {
  plot_tibble <- unnest(calc_coverage_per_X(pred_tibble), conditional)
  ggplot(plot_tibble, aes(x = X, y = conditional_coverage, color = method_name)) +
    ## geom_ribbon(aes(ymin = conditional_coverage - cc_std, ymax = conditional_coverage + cc_std, fill = method_name), alpha = 0.2) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Predictions by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Coverage",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}


plot_cond_sd <- function(pred_tibble) {
  calc_coverage_per_X(pred_tibble) |>
    unnest(conditional) |>
    ggplot(aes(x = X, y = cc_std, color = method_name)) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "SD by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional SD",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}

## TODO 2024-06-07 Summarise results for conditional coverage in table.


## -----------------------------------------------------------------------------
## Run simulation
## -----------------------------------------------------------------------------

runs <- 500

alpha_sig <- 0.1

results_tibble <- crossing(
  tibble(n = 2^(5:16)),
  tibble(model = list(
    D = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) + 10 * (x >= 5),
    P = function(x) rpois(length(x), pmin(pmax(x, 1), 6)),
    NI = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 *(x > 7),
    S = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6))
  )),
  tibble(method = list(
    QR = dcp_qr,
    DR = dcp_dr,
    IDR = dcp_idr,
    CP_OLS = dcp_cp_ols,
    CP_LOC = dcp_cp_loc
  ))) |>
  mutate(model_name = names(model), method_name = names(method), .after = n) |>
  mutate(compute = pmap(list(n, model_name, model, method_name, method), run_simulation)) |>
  unnest(compute)

save(results_tibble, file = file.path(results_dir, "results_tibble.RData")) 
