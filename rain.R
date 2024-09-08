library(data.table)
library(dplyr)
library(furrr)
library(lubridate)
library(parallel)
library(purrr)
library(stringr)
library(tidyr)

source("dcp.R")
alpha_sig <- 0.1

### Load data ------------------------------------------------------------------

precipitation <- readRDS("data/precipitation_data.rds")
precipitation[, mptb := rowMeans(.SD), .SDcols = paste0("p", 1:50)]


### Functions ------------------------------------------------------------------

generate_indices_dcp <- function(n, run) {
  ## Generates a list with keys `train_ind', `valid_ind' and `test_ind'.
  ## such that they cover 60% of the total data
  ## and correspond to `run' (∈ {1, …, 5}).

  train_valid_test <- function(n) {
    ## Equal train–validation split
    ## Use ≈ 16% for testing
    ## That way, we use 50% of the data for training+validation and 10% for testing,
    ## as `generate_sequential_indices' passes 60% of the total data to this function.
    list(train_ind = 1:floor(0.42 * n),
      valid_ind = (floor(0.42 * n) + 1):(floor(0.84 * n)),
      test_ind = (floor(0.84 * n) + 1):n)
  }

  train_valid_test(floor(0.6 * n)) |>
    lapply(\(indices) floor((run - 1) * n / 10) + indices)
}


generate_indices_ziegel <- function(n, run) {
  ## Generate indices the way Prof. Ziegel suggested, ie,
  ## do not average across multiple sets in time.
  list(train_ind = 1:floor(0.4*n),
    valid_ind = (floor(0.4*n)+1):floor(0.8*n),
    test_ind = (floor(0.8*n)+1):n)
}


run_analysis <- function(airport, horizon, method, indices, variant) {
  summarise_analysis <- function(dt) {
    dt[, {
      ## Estimate the unconditional coverage and length as the overall mean.
      coverage <- mean(conditional_coverage, na.rm = TRUE)
      leng <- mean(conditional_leng, na.rm = TRUE)

      ## For conditional coverage, also train a GLM on the covariates and add its
      ## predictions to the summary
      glm <- reformulate(termlabels = termlabels, response = "conditional_coverage")
      fm <- glm(glm, data = .SD, family = binomial(link = "logit"))
      pred <- predict(fm, newdata = .SD, type = "response")

      list(
        coverage = coverage,
        leng = leng,
        conditional = .(.SD[, .(date, coverage = conditional_coverage, pred = pred, leng = conditional_leng)])
      )
    }]
  }

  termlabels <- switch(variant,
    "cw" = c("hres", "ctr", "mptb"),
    "icx" = c("hres", paste0("p", 1:50))
  )

  form <- reformulate(termlabels = termlabels, response = "obs")

  ## Adapted from https://github.com/AlexanderHenzi/isodistrreg
  ## Variable selection: use HRES and the perturbed forecasts P1, ..., P50
  ## Partial orders on variable groups: Usual order of numbers on HRES (group '1'),
  ## increasing convex order on the remaining variables (group '2').
  groups <- setNames(switch(variant,
    "cw" = rep(1, 3),
    "icx" = c(1, rep(2, 50))
  ),
  termlabels)
  orders <- switch(variant,
    "cw" = c("comp" = 1),
    c("comp" = 1, "icx" = 2)
  )

  dt <- precipitation[airport == ap & horizon == hz,
    env = list(ap = I(airport), hz = I(horizon))]

  with(indices, {
    message(str_c(airport, horizon, method, train_ind[1], sep = " "))
    start <- Sys.time()
    dt <- dcp(method, form, dt[train_ind], dt[valid_ind], dt[test_ind],
      alpha_sig = alpha_sig,
      method = "fn", # QR(*) arguments
      groups = groups, orders = orders # IDR(*) arguments
    )
    elapsed <- difftime(Sys.time(), start, units = "secs")
    message(str_c("Time:", format(round(elapsed, 3)), sep = " "))
    summarise_analysis(dt)
  })
}


### Analysis -------------------------------------------------------------------

configs <- expand_grid(approach = c("dcp", "ziegel"), variant = c("cw", "icx")) |>
  purrr::transpose()
methods <- c("CP_LOC", "CP_OLS", "DR", "IDR", "IDR*", "QR", "QR*")

numCores <- detectCores() - 1

dir <- file.path("results", "precipitation",  format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

for (config in configs) {
  message(str_c(config$approach, config$variant, sep = " "))

  runs <- switch(config$approach, "dcp" = 1:5, "ziegel" = 1)
  indexFn <- switch(config$approach,
    "dcp" = generate_indices_dcp,
    "ziegel" = generate_indices_ziegel)

  start <- Sys.time()

  ## Create evaluation grid
  result <- precipitation[, .(n = .N), keyby = .(airport, horizon)] |>
    expand_grid(method = methods, run = runs) |>
    mutate(indices = map2(n, run, indexFn), .keep = "unused") |>
    ## Perform calculations.
    mutate(compute =
             mcmapply(
               run_analysis, airport, horizon, method, indices, config$variant,
               mc.cores = numCores,
               SIMPLIFY = FALSE)) |>
    unnest(compute) |>
    as.data.table()

  elapsed <- difftime(Sys.time(), start, units = "secs")
  message(str_c("Time:", format(round(elapsed, 3)), sep = " "))

  result[, indices := NULL]

  result <- result[,
    ## Mean over first three statistics
    c(lapply(.SD[, c("coverage", "leng", "ccmse")], mean),
      ## Summarise conditional coverage/length by stitching them together
      .(conditional = .(rbindlist(.SD$conditional)))),
    by = .(airport, horizon, method),
    ]

  saveRDS(result, file = file.path(dir, str_c(str_c("results", config$approach, config$variant, sep = "_"), ".rds")))
}
