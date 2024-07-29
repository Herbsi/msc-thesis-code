library(data.table)
library(dplyr, warn.conflicts=FALSE)
library(dtplyr)
library(lubridate, warn.conflicts=FALSE)
library(purrr, warn.conflicts=FALSE)
library(tibble, warn.conflicts=FALSE)
library(tidyr, warn.conflicts=FALSE)

source("dcp.R")

load("data/precipData_caseStudy.rda")
precipitation <- as_tibble(precipData_caseStudy)

## Filter out days that do not occur at every location

incomplete_days <- precipitation |>
  group_by(date) |>
  summarise(count = n()) |>
  filter(count < 20) |>
  pull(date)

precipitation <- precipitation |>
  filter(!(date %in% incomplete_days))


train_valid_test <- function(n) {
  ## Equal train–validation split
  ## Use ≈ 16% for testing
  ## That way, we use 50% of the data for training+validation and 10% for testing,
  ## as `generate_sequential_indices' passes 60% of the total data to this function.
  list(train_ind = 1:floor(0.42 * n),
    valid_ind = (floor(0.42 * n) + 1):(floor(0.84 * n)),
    test_ind = (floor(0.84 * n) + 1):n)
}

generate_sequential_indices <- function(n) {
  replicate(5, train_valid_test(floor(0.6 * n)), simplify = FALSE) |>
    imap(\(split, idx) map(split, ~ floor((idx - 1) * n / 10) + .x))
}

run_analysis <- function(.data, method_name, train_ind, valid_ind, test_ind) {
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
}

summarise_analysis <- function(.data) {
  glm_form <- reformulate(termlabels =  names(precipitation)[-(1:4)], response = "conditional_coverage")
  fm <- glm(glm_form, data = .data, family = binomial(link = "logit"))
  pred <- predict(fm, newdata = .data, type = "response")
  cc_mse <- sqrt(mean((pred - 0.9)^2))
  
  .data |>
    summarise(unconditional_coverage = mean(conditional_coverage),
      average_leng = mean(conditional_leng, na.rm = TRUE),
      conditional_coverage_mse = cc_mse,
      conditional_leng_sd = sd(.data$conditional_leng, na.rm = TRUE)
    )
}

process_data <- function(.data, key, debug = FALSE) {
  print(key)
  method_name <- if(debug) names(dcp_method_list) else names(dcp_method_list)
  indices <- generate_sequential_indices(if(debug) 1000 else nrow(.data))
  ## Generate all combinations of `method' to apply, `run' = 1 … 5 and corresponding `train', `valid' and `test' indices.
  crossing(method_name = method_name, indices = indices) |>
    mutate(run = rep(1:5, times = if(debug) 7 else length(dcp_method_list)), .before = indices) |>
    unnest_wider(indices) |>
    ## At this point, the tibble looks like:
    ## method_name  run  train_ind  valid_ind  test_ind
    ##   CP_OLS       1  <dbl [ ]>  <dbl [ ]>  <dbl [ ]>
    ##   ...
    ## For each such row, we …
    pmap(\(method_name, run, train_ind, valid_ind, test_ind) {
      with(summarise_analysis(run_analysis(.data, method_name, train_ind, valid_ind, test_ind)),
        list(method_name = method_name, # … remember the method
          unconditional_coverage = unconditional_coverage, # and the summarised performance metrics.
          average_leng = average_leng,
          conditional_coverage_mse = conditional_coverage_mse,
          conditional_leng_sd = conditional_leng_sd)
      )
    }, .progress = TRUE) |> # We bind together the results of a single group into a data.table.
    rbindlist()
}


result_summarised <- lazy_dt(precipitation) |>
  group_by(airport, horizon) |> # Do the same for every combination of `airport' and `horizon'
  group_modify(process_data, debug = FALSE) |> # `debug' gets passed to `process_data'.
  ## Since `process_data' returns a `data.frame', at this point we are still a grouped data frame
  ## So we additionally group by `method_name' and then average over the five runs.
  group_by(method_name, .add = TRUE) |>
  summarise(unconditional_coverage = mean(unconditional_coverage),
    average_leng = mean(average_leng),
    conditional_coverage_mse = mean(conditional_coverage_mse),
    conditional_leng_sd = mean(conditional_leng_sd)) |>
  ungroup() |>
  as_tibble()


