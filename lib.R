## Packages
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")

## Analysis --------------------------------------------------------------------

analyse <- function(data_frame, split, sig = 0.1) {
  tibble(
    name = c("dcp-qr",  "dcp-dr", "dcp-idr", "dcp-idrbag", "cp-ols", "cp-loc"),
    fn = list(dcp_qr, dcp_dr, dcp_idr, dcp_idrbag, dcp_cp_ols, dcp_cp_loc),
    results = map(seq_along(name), ~ tibble(coverage = NULL, leng = NULL))
  ) |>
    mutate(results = map(fn, \(f) f(Y ~ X, data_frame, split, sig)))
}

evaluate <- function(data_frame) {
  data_frame |>
    mutate(
      ## TODO <2024-05-28 Tue>: Add `sd' and `avg length of IV'
      unconditional_coverage = map_dbl(results, \(df) summarise(df, mean(coverage)) |> pull()),
    )
}

## Plotting --------------------------------------------------------------------
         
bin <- function(data, base, num_bins) {
  ## TODO <2024-05-22 Wed> Currently, the final bin is odd.
  ## Bins data into `num_bins'-many bins.
  ## Each bin corresponds to a quantile bin of `base'.
  ## This assumes that the rows of `data' and `base' correspond to the same objecs.
  data |>
    mutate(base_rank = rank(base, ties.method = "random")) |>
    mutate(bin = floor((num_bins - 1) * base_rank / nrow(data)), .keep = "unused") |>
    group_by(bin)
}


## Simulation ------------------------------------------------------------------

run_simulation <- function(n, model_name, model, method_name, method, runs, alpha_sig, results_dir) {
  writeLines(str_c(n, model_name, method_name, sep = " "))
  
  ## Setup
  set.seed(42 + n) # Ensures we generate the same data for every `n'
  
  n_train <- n / 2
  n_valid <- n / 4
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

  prediction_interval <- seq(0, 10, length.out = 100)

  simulation_result <- replicate(runs, simplify = FALSE, expr = {
    data_tibble <- tibble(
      X = runif(n, 0, 10),
      Y = model(X)
    ) |>  
      mutate( # Add noise to training data.
        X = X + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train)),
        Y = Y + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train))
      )
    
    with(method(Y ~ X, data_tibble, split, alpha_sig),
      tibble(
        coverage = mean(coverage),
        leng = leng,
        conditional_coverage = list(predict_from_tidy(conditional_coverage, prediction_interval)),
        conditional_leng = list(predict(conditional_leng, prediction_interval)$y)))
  }) |>
  list_rbind() |>
  summarise(
    coverage = mean(coverage),
    leng = mean(leng),
    ## `conditional_coverage' is a list of `runs' items, each a vector of 100 values;
    ## (the 100 comes from `prediction_interval')
    ## bind_cols turns it into a 100 x `runs' tibble;
    ## then we take the `rowMeans()' to average out the data randomness.
    conditional_coverage = bind_cols(conditional_coverage, .name_repair = "unique") |> rowMeans() |> list(),
    conditional_leng = bind_cols(conditional_leng, .name_repair = "unique") |> rowMeans() |> list()
  )

  filename <- file.path(results_dir, str_c(n, model_name, method_name, sep = "_") |> str_c(".RData"))
  ## Save results to file
  save(simulation_result, file = filename)

  ## Return result
  simulation_result
}


theorem_3 <- function(runs = 500, alpha_sig = 0.1, n = 2^(5:16)) {
  current_time <- format(Sys.time(), "%Y%m%d%H%M%S")

  results_dir <- file.path("results", "theorem-3", current_time)
  dir.create(results_dir)

  results_tibble <- crossing(
    ## Create tibble with all combinations of `n', `model' and `method'
    tibble(n = n),
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
    ## Add column with model name and column with method name
    mutate(model_name = names(model), method_name = names(method)) |>
    ## Add temporary columns with `runs' and `results_dir' for `run_simulation'
    mutate(runs = runs, alpha_sig = alpha_sig, results_dir = results_dir) |> 
    mutate(compute = pmap(across(everything()), run_simulation)) |>
    ## Only keep useful columns
    select(n, model_name, method_name, compute) |>
    unnest(compute)

  save(results_tibble, file = file.path(results_dir, "results_tibble.RData"))

  results_tibble
}


merge_results <- function(n, ts) {
  df <- crossing(
    tibble(n = n),
    tibble(model_name = c("D", "P", "NI", "S")),
    tibble(method_name = c("QR", "DR", "IDR", "CP_OLS", "CP_LOC"))
  )
  files <- str_c("results/theorem-3-euler/", ts, "/",
    str_c(df$n, df$model_name, df$method_name, sep = "_")) |>
    str_c(".RData")

  df2 <- tibble()
  walk(files, \(f) {
    load(force(f), verbose = TRUE)
    df2 <<- bind_rows(df2, tribble(~coverage, ~leng, ~conditional, simulation_result$coverage, simulation_result$leng, simulation_result$conditional))
  }
  )

  results_tibble <- bind_cols(df, df2) |> mutate(conditional = map(conditional, ~ .x[[1]]))
  save(results_tibble, file = str_c("results/theorem-3-euler/results_tibble", format(Sys.time(), "%Y%m%d%H%M%S"), ".RData"))
  results_tibble
}


predict_from_tidy <- function(tidy_model, x_values) {
  intercept <- tidy_model |> filter(term == "(Intercept)") |> pull(estimate)
  slope <- tidy_model |> filter(term == "X") |> pull(estimate)
  linear_predictor <- intercept + slope * x_values
  plogis(linear_predictor)
}


pred_conditional <- function(results_tibble) {
  ## Turns `results_tibble' into `pred_tibble' by predicting conditional coverage
  ## and conditional length along a grid of X values.

  prediction_interval <- seq(0, 10, length.out = 100)

  results_tibble |>
    mutate(conditional = map2(conditional_coverage, conditional_leng, # Map the column
      \(glm_fits, spline_fits) {
        map2(glm_fits, spline_fits, # Map the lists in the current row
          \(glm_fit, spline_fit) {
            data.frame(
              X = prediction_interval,
              coverage = predict_from_tidy(glm_fit, prediction_interval),
              leng = spline_fit(prediction_interval))
          })
      }), .keep = "unused")
}


calc_conditional_per_X <- function(pred_tibble) {
  pred_tibble |>
    mutate(conditional = map(conditional,
      \(tibble_list) {
        bind_rows(tibble_list) |>
          group_by(X) |>
          summarise(
            coverage = mean(coverage),
            coverage_sd = sd(coverage),
            leng = mean(leng),
            leng_sd = sd(leng)
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


