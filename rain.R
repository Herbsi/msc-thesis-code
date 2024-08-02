library(data.table)
library(dplyr, include.only = c("mutate"))
library(purrr, include.only = c("map2"))
library(stringr, include.only = c("str_c"))
library(tidyr, include.only = c("expand_grid"))

source("dcp.R")
alpha_sig <- 0.1

### Load data ------------------------------------------------------------------

precipitation <- readRDS("data/precipitation_data.rds")

## Filter out days that do not occur at every location
## We have 4 locations and 5 horizons, so every date needs to occur 20 times.
precipitation <- precipitation[precipitation[, .N, by = date][N == 20], on = "date"][, -c("N")]


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


run_analysis <- function(airport, horizon, method, indices) {
  message(str_c(airport, horizon, method, sep = " "))
  
  dt <- precipitation[airport == ap & horizon == hz,
    env = list(ap = I(airport), hz = I(horizon))]

  ## FIXME 2024-08-02 `indices' requires this weird unpacking.
  train_ind <- indices[[1]][[1]]
  valid_ind <- indices[[1]][[2]]
  test_ind <- indices[[1]][[3]]

  form <- reformulate(termlabels =  names(precipitation)[-c((1:4), 6)], response = "obs")

  ## Adapted from https://github.com/AlexanderHenzi/isodistrreg
  ## Variable selection: use HRES and the perturbed forecasts P1, ..., P50
  varNames <- names(precipitation)[-c((1:4), 6)]

  ## Partial orders on variable groups: Usual order of numbers on HRES (group '1'),
  ## increasing convex order on the remaining variables (group '2').
  groups <- setNames(c(1, rep(2, 50)), varNames)
  orders <- c("comp" = 1, "icx" = 2)

  dcp(method, form, dt[train_ind], dt[valid_ind], dt[test_ind], alpha_sig = alpha_sig, groups = groups, orders = orders)
}


summarise_analysis <- function(dt) {
  glm_form <- reformulate(termlabels = names(precipitation)[-c((1:4), 6)],
    response = "conditional_coverage")
  fm <- glm(glm_form, data = dt, family = binomial(link = "logit"))
  pred <- predict(fm, newdata = dt, type = "response")
  cc_mse <- sqrt(mean((pred - (1 - alpha_sig))^2)) 

  list(## Estimate the unconditional coverage as the overall mean.
    coverage = mean(dt$conditional_coverage, na.rm = TRUE),
    ## Similarly for the length.
    leng = mean(dt$conditional_leng, na.rm = TRUE),
    ## Summarise the conditional length by binning it according to X
    conditional_coverage_mse = cc_mse,
    conditional_leng_sd = sd(dt$conditional_leng, na.rm = TRUE)
  )
}


### Analysis -------------------------------------------------------------------

dir <- "results/precipitation/"
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

configs <- list(
  list(name = "dcp",
    runs = 1:5,
    indices = generate_indices_dcp),
  list(name = "ziegel",
    runs = 1,
    indices = generate_indices_ziegel)
)
methods <- c("CP_LOC", "CP_OLS", "DR", "IDR", "IDR*", "QR", "QR*")
columns <- c("coverage", "leng", "conditional_coverage_mse", "conditional_leng_sd")

for (config in configs) {
  message(config$name)
  ## Create evaluation grid
  result <- precipitation[, .(n = .N), keyby = .(airport, horizon)] |>
    expand_grid(method = methods, run = config$runs) |>
    mutate(indices = map2(n, run, config$indices), .keep = "unused") |>
    as.data.table()

  ## Perform calculations.
  result[,
  (columns) := summarise_analysis(run_analysis(airport, horizon, method, indices)),
  by = .I][, indices := NULL]

  ## Mean over runs.
  result <- result[,
    lapply(.SD, mean),
    by = .(airport, horizon, method),
    .SDcols = columns
  ]

  saveRDS(result, file = file.path(dir, str_c("result", config$name, ".rds")))
}
