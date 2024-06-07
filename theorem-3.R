## Packages
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")

logs_dir <- "logs/"
results_dir <- "results/theorem-3/"
runs <- 500

alpha_sig <- 0.1

run_simulation <- function(n, model_name, model, method_name, method) {
  log_file <- str_c(logs_dir, "theorem-3.log")
  ## Setup
  set.seed(42 + n) # Ensures we generate the same data for every `n'
  cat(str_c(n, model_name, method_name, sep = " ") , file = log_file, sep = "\n", append = TRUE)

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

  simulation_result <- reduce(1:runs, \(acc, nxt) {
    data_tibble <- tibble(
      X = runif(n, 0, 10),
      Y = model(X)
    ) |>  
      mutate( # Add noise to training data.
        X = X + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train)),
        Y = Y + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train))
      )
    
    results_list <- method(Y ~ X, data_tibble, split, alpha_sig)
    cat(str_c("Run", nxt, sep = " "), file = log_file, sep = "\n", append = TRUE)

    acc$coverage <- acc$coverage + (mean(results_list$coverage) - acc$coverage) / nxt
    acc$leng <- acc$leng + (mean(results_list$leng) - acc$leng) / nxt
    acc$conditional <- append(acc$conditional, list(tidy(results_list$conditional_glm)))
    acc
  },
  .init = list(
    coverage = 0,
    leng = 0,
    conditional = list()
  ))

  ## Save results to file
  filename <- results_dir |>
    str_c(n, model_name, method_name, sep = "_") |>
    str_c(".RData")
  save(simulation_result, file = filename)

  ## Return result
  simulation_result
}

plot_coverage <- function(results_tibble) {
  log4_trans <- trans_new("log4", 
    transform = function(x) log(x, base = 4),
    inverse = function(x) 4^x)

  ggplot(results_tibble, aes(x = n, y = coverage, color = method_name, group = method_name)) +
    geom_line() +
    facet_wrap(~ model_name) +
    labs(title = "Coverage vs n for each Method and Model",
      x = "n",
      y = "Coverage",
      color = "Method") +
    ## scale_x_continuous(breaks = unique(results_tibble$n)) +
    scale_x_continuous(trans = log4_trans) +
    theme_minimal()
}

plot_leng <- function(results_tibble) {
  log4_trans <- trans_new("log4", 
    transform = function(x) log(x, base = 4),
    inverse = function(x) 4^x)

  ggplot(results_tibble, aes(x = n, y = leng, color = method_name, group = method_name)) +
    geom_line() +
    facet_wrap(~ model_name) +
    labs(title = "Leng vs n for each Method and Model",
      x = "n",
      y = "Coverage",
      color = "Method") +
    ## scale_x_continuous(breaks = unique(results_tibble$n)) +
    scale_x_continuous(trans = log4_trans) +
    theme_minimal()
}

plot_cond_coverage <- function(results_tibble) {
  ## Function to generate predictions using coefficients from tidy output
  predict_from_tidy <- function(tidy_model, x_values) {
    intercept <- tidy_model |> filter(term == "(Intercept)") |> pull(estimate)
    slope <- tidy_model |> filter(term == "X") |> pull(estimate)
    linear_predictor <- intercept + slope * x_values
    plogis(-linear_predictor)
  }
  
  prediction_interval <- seq(0, 10, length.out = 100)
  
  plot_tibble <- results_tibble |>
    mutate(conditional =
             map(conditional,
               \(model_list) {
                 map(model_list,
                   \(tidy_model) {
                     data.frame(
                       X = prediction_interval,
                       prediction = predict_from_tidy(tidy_model, prediction_interval))
                   }) |>
                   bind_rows() |>
                   group_by(X) |>
                   summarise(
                     conditional_coverage = mean(prediction),
                     cc_std = sd(prediction)
                     )
               }), .keep = "unused") |>
    unnest(conditional)
  
  ggplot(plot_tibble, aes(x = X, y = conditional_coverage, color = method_name)) +
    geom_ribbon(aes(ymin = conditional_coverage - cc_std, ymax = conditional_coverage + cc_std, fill = method_name), alpha = 0.2) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Predictions by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Coverage",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}
## -----------------------------------------------------------------------------
## Run simulation
## -----------------------------------------------------------------------------

results_tibble <- crossing(
  ## tibble(n = 4^(3:5)),
  tibble(n = 4^(3:10)),
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
  mutate(
    coverage = map_dbl(compute, \(x) x$coverage),
    leng = map_dbl(compute, \(x) x$leng),
    conditional = map(compute, \(x) x$conditional),
    .keep = "unused"
  )

save(results_tibble, file = "results/theorem-3/result_tibble.RData")
