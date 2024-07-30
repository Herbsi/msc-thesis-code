library(data.table)
library(dplyr, warn.conflicts=FALSE)
library(dtplyr)
library(furrr)
library(lubridate, warn.conflicts=FALSE)
library(purrr, warn.conflicts=FALSE)
library(stringr)
library(tibble, warn.conflicts=FALSE)
library(tidyr, warn.conflicts=FALSE)

source("dcp.R")

load("data/precipData_caseStudy.rda")
precipitation <- lazy_dt(precipData_caseStudy)

## Filter out days that do not occur at every location

incomplete_days <- precipitation |>
  group_by(date) |>
  summarise(count = n()) |>
  filter(count < 20) |>
  pull(date)

precipitation <- precipitation |>
  filter(!(date %in% incomplete_days)) |>
  as.data.table()


train_valid_test <- function(n) {
  ## Equal train–validation split
  ## Use ≈ 16% for testing
  ## That way, we use 50% of the data for training+validation and 10% for testing,
  ## as `generate_sequential_indices' passes 60% of the total data to this function.
  list(train_ind = 1:floor(0.42 * n),
    valid_ind = (floor(0.42 * n) + 1):(floor(0.84 * n)),
    test_ind = (floor(0.84 * n) + 1):n)
}

generate_indices <- function(n, run) {
  ## Generates a list with keys `train_ind', `valid_ind' and `test_ind'.
  ## such that they cover 60% of the total data
  ## and correspond to `run' (∈ {1, …, 5}).
  train_valid_test(floor(0.6 * n)) |>
    map(\(indices) floor((run - 1) * n / 10) + indices)
}

run_analysis <- function(.data, method_name, indices) {
  with(indices, {
    form <- reformulate(termlabels =  names(precipitation)[-c((1:4), 6)], response = "obs")
    data_train <- as.data.table(.data)[train_ind, ]
    data_valid <- as.data.table(.data)[valid_ind, ]
    data_test <- as.data.table(.data)[test_ind, ]

    ## Adapted from https://github.com/AlexanderHenzi/isodistrreg
    ## Variable selection: use HRES and the perturbed forecasts P1, ..., P50
    varNames <- names(precipitation)[-c((1:4), 6)]

    ## Partial orders on variable groups: Usual order of numbers on HRES (group '1'),
    ## increasing convex order on the remaining variables (group '2').
    groups <- setNames(c(1, rep(2, 50)), varNames)
    orders <- c("comp" = 1, "icx" = 2)
    
    dcp_method_list[[method_name]](form, data_train, data_valid, data_test, alpha = 0.1, groups = groups, orders = orders)
  })
}

summarise_analysis <- function(.data) {
  glm_form <- reformulate(termlabels =  names(precipitation)[-c((1:4), 6)], response = "conditional_coverage")
  fm <- glm(glm_form, data = .data, family = binomial(link = "logit"))
  pred <- predict(fm, newdata = .data, type = "response")
  cc_mse <- sqrt(mean((pred - 0.9)^2)) # NOTE 2024-07-30 Hard-coded `0.9' (= 1 - α)
  
  .data |>
    summarise(unconditional_coverage = mean(conditional_coverage),
      average_leng = mean(conditional_leng, na.rm = TRUE),
      conditional_coverage_mse = cc_mse,
      conditional_leng_sd = sd(.data$conditional_leng, na.rm = TRUE)
    )
}

plan(multicore, workers = availableCores(constraints = "multicore"))

rain_results <- precipitation |>
  group_by(airport, horizon) |>
  summarise(n = n(), .groups = "drop") |>
  expand_grid(method_name = names(dcp_method_list), run = 1:5) |>
  mutate(indices = map2(n, run, generate_indices)) |>
  mutate(compute = future_pmap(list(airport, horizon, method_name, run, indices),
    \(airport, horizon, method_name, run, indices) {
      writeLines(str_c(airport, horizon, method_name, run, sep = " "))
      ap <- airport
      hz <- horizon
      with(summarise_analysis(run_analysis(precipitation[airport == ap & horizon == hz], method_name, indices)), {
        list(unconditional_coverage = unconditional_coverage,
          average_leng = average_leng,
          conditional_coverage_mse = conditional_coverage_mse,
          conditional_leng_sd = conditional_leng_sd)
      })
    }, .progress = TRUE)) |>
  unnest_wider(compute) |>
  group_by(airport, horizon, method_name) |>
  summarise(unconditional_coverage = mean(unconditional_coverage),
    average_leng = mean(average_leng),
    conditional_coverage_mse = mean(conditional_coverage_mse),
    conditional_leng_sd = mean(conditional_leng_sd)) |>
  ungroup() 


save(precipitation_results_summarised, file = "results/precipitation/results_parallel.RData")

