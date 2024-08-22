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


run_analysis <- function(airport, horizon, method, indices, config) {
  summarise_analysis <- function(dt) {
    glm_full <- reformulate(termlabels = config$termlabels,
      response = "conditional_coverage")
    fm_full <- glm(glm_full, data = dt, family = binomial(link = "logit"))
    pred_full <- predict(fm_full, newdata = dt, type = "response")
    cc_mse_full <- sqrt(mean((pred_full - (1 - alpha_sig))^2)) 

    fm_hres <- glm(conditional_coverage ~ hres, data = dt, family = binomial(link = "logit"))
    pred_hres <- predict(fm_hres, newdata = dt, type = "response")
    cc_mse_hres <- sqrt(mean((pred_hres - (1 - alpha_sig))^2)) 

    list(## Estimate the unconditional coverage as the overall mean.
      coverage = mean(dt$conditional_coverage, na.rm = TRUE),
      ## Similarly for the length.
      leng = mean(dt$conditional_leng, na.rm = TRUE),
      conditional_coverage_mse_full = cc_mse_full,
      conditional_coverage_mse_hres = cc_mse_hres,
      conditional_coverage = dt[, .(date, hres,
        conditional_coverage_full = pred_full, conditional_coverage_hres = pred_hres)],
      conditional_leng = dt[, .(date, hres, conditional_leng)]
    )
  }
  form <- reformulate(termlabels = config$termlabels, response = "obs")

  ## Adapted from https://github.com/AlexanderHenzi/isodistrreg
  ## Variable selection: use HRES and the perturbed forecasts P1, ..., P50
  ## Partial orders on variable groups: Usual order of numbers on HRES (group '1'),
  ## increasing convex order on the remaining variables (group '2').
  groups <- setNames(c(1, rep(2, length(config$termlabels) - 1)), config$termlabels)
  orders <- if(length(config$termlabels) > 1) {
    c("comp" = 1, "icx" = 2)
  } else {
    c("comp" = 1)
  }
  
  dt <- precipitation[airport == ap & horizon == hz,
    env = list(ap = I(airport), hz = I(horizon))]

  with(indices, {
    message(str_c(airport, horizon, method, train_ind[1], sep = " "))
    start <- Sys.time()
    dt <- summarise_analysis(dcp(method, form, dt[train_ind], dt[valid_ind], dt[test_ind],
      alpha_sig = alpha_sig,
      method = "fn", # QR(*) arguments
      groups = groups, orders = orders # IDR(*) arguments
    ))
    elapsed <- difftime(Sys.time(), start, units = "secs")
    message(str_c("Time:", format(round(elapsed, 3)), sep = " "))
    
    subDir <- file.path(dir, config$name)
    dir.create(subDir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(dt, file = file.path(subDir, str_c(str_c(airport, horizon, method,
      train_ind[1], sep = "_"), ".rds")))
    
    dt
  })
}


### Analysis -------------------------------------------------------------------

configs <- list(
  list(name = "dcp_full",
    runs = 1:5,
    indices = generate_indices_dcp,
    termlabels = names(precipitation)[-c((1:4), 6)]),
  list(name = "dcp_hres",
    runs = 1:5,
    indices = generate_indices_dcp,
    termlabels = c("hres")),
  list(name = "ziegel_full",
    runs = 1,
    indices = generate_indices_ziegel,
    termlabels = names(precipitation)[-c((1:4), 6)]),
  list(name = "ziegel_hres",
    runs = 1,
    indices = generate_indices_ziegel,
    termlabels = c("hres"))

)
methods <- c("CP_LOC", "CP_OLS", "DR", "IDR", "IDR*", "QR", "QR*")

numCores <- detectCores() - 1

dir <- file.path("results", "precipitation",  format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

for (config in configs) {
  message(config$name)
  
  start <- Sys.time()
  
  ## Create evaluation grid
  result <- precipitation[, .(n = .N), keyby = .(airport, horizon)] |>
    expand_grid(method = methods, run = config$runs) |>
    mutate(indices = map2(n, run, config$indices), .keep = "unused") |> 
    ## Perform calculations.
    mutate(compute = mcmapply(run_analysis, airport, horizon, method, indices,
      mc.cores = numCores,
      MoreArgs = list(config = config), SIMPLIFY = FALSE)) |> 
    unnest_wider(compute) |>
    as.data.table()

  elapsed <- difftime(Sys.time(), start, units = "secs")
  message(str_c("Time:", format(round(elapsed, 3)), sep = " "))
  
  result[, indices := NULL]

  result <- result[,
    ## Mean over first three statistics
    c(lapply(.SD[, c("coverage", "leng", "conditional_coverage_mse_full", "conditional_coverage_mse_hres")], mean),
      ## Summarise conditional coverage/length by stitching it them together
      .(conditional_coverage = .(rbindlist(.SD$conditional_coverage))),
      .(conditional_leng = .(rbindlist(.SD$conditional_leng)))),
    by = .(airport, horizon, method),
    ]

  saveRDS(result, file = file.path(dir, str_c("result", config$name, ".rds")))
}
