## Packages
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

source("dcp.R")
source("lib.R")

alpha.sig <- 0.1

results_tibble <- crossing(
  tibble(n = 4^(3:5)),
  ## tibble(n = 4^(3:10)),
  tibble(model = list(
    ## D = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) + 10 * (x >= 5),
    ## P = function(x) rpois(length(x), pmin(pmax(x, 1), 6)),
    ## NI = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 *(x > 7),
    S = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6))
  )),
  tibble(method = list(
    QR = dcp_qr,
    DR = dcp_dr,
    IDR = dcp_idr,
    CP_OLS = dcp_cp_ols,
    CP_LOC = dcp_cp_loc
  )
  )
)

run_simulation <- function(n, model, method) {
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

  ## TODO <2024-05-29 Wed> Use 500 here.
  ## Take mean coverage and length over 500 runs
  reduce(1:10, \(acc, nxt) {
    data_tibble <- tibble(
      X = runif(n, 0, 10),
      Y = model(X)
    ) |>  
      mutate( # Add noise to training data.
        X = X + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train)),
        Y = Y + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train))
      )
    
    result <- method(Y ~ X, data_tibble, split, alpha.sig)

    res <- result |> summarise(coverage = mean(coverage), leng = mean(leng))

    ## TODO <2024-05-29 Wed> DCP paper does some GLM related stuff.
    cond_res <- result |>
      mutate(bins = cut(data_tibble$X[ind_test], breaks = 0:10)) |>
      group_by(bins, .drop = FALSE) |>
      summarise(
        conditional_coverage = mean(coverage),
        conditional_length = mean(leng)
      )

    ## TODO <2024-05-29 Wed> Make accumulation more elegant somehow.
    acc$coverage <- acc$coverage + (res$coverage - acc$coverage) / nxt
    acc$leng <- acc$leng + (res$leng - acc$leng) / nxt
    ## TODO <2024-05-29 Wed> Deal with NaNs (happens when a bin is empty)
    ## TODO <2024-05-29 Wed> Make selecting columns more generic.
    acc$conditional <- cbind(bins = acc$conditional[, 1],
      acc$conditional[, c(2:3)] + (cond_res[, c(2:3)] - acc$conditional[, c(2:3)]) / nxt
    )
    acc
  },
  .init = list(
    coverage = 0,
    leng = 0,
    conditional = tibble(
      bins = levels(cut(1:10, breaks = 0:10)),
      conditional_coverage = rep(0, 10),
      conditional_leng = rep(0, 10)
    )
  ))
}

results_tibble <- results_tibble |> # I did too much Rust coding back in my days.
  mutate(compute = pmap(list(n, model, method), run_simulation)) |>
  mutate(
    coverage = map_dbl(compute, \(x) x$coverage),
    leng = map_dbl(compute, \(x) x$leng),
    ## TODO <2024-05-29 Wed> Maybe unpack conditional differently.
    conditional = map(compute, \(x) x$conditional),
    .keep = "unused"
  )

## TODO <2024-05-29 Wed>: Maybe also dump raw computed data
## TODO <2024-05-29 Wed>: Save `results' after computation
