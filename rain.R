library(dplyr, warn.conflicts=FALSE)
library(lubridate, warn.conflicts=FALSE)
library(purrr)
library(tibble)

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


## We have the following relation in the data (although floating point numbers don't make this obvious)
## (`airport', `date') => `obs', ie `airport' and `date' determine `obs'

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
  form <- reformulate(termlabels =  names(precipitation)[-(1:4)], response = "obs")
  data_train <- as.data.table(.data)[train_ind, ]
  data_valid <- as.data.table(.data)[valid_ind, ]
  data_test <- as.data.table(.data)[test_ind, ]
  dcp_method_list[[method_name]](form, data_train, data_valid, data_test) |>
                                 ## NOTE 2024-07-29 Currently, we remove the data columns from the results to make them smaller.
                                 select(starts_with("conditional"))
}

precipitation |>
  group_by(airport, horizon) |> # Do the same for every combination of `airport' and `horizon'
  group_map(\(.data, key) {
    print(key)
    ## Generate all combinations of `method' to apply, `run' = 1 … 5 and corresponding `train', `valid' and `test' indices.
    crossing(method_name = names(dcp_method_list), indices = generate_sequential_indices(100)) |>
      mutate(run = rep(1:5, times = length(dcp_method_list)), .before = indices) |>
      unnest_wider(indices) |>
      ## At this point, the tibble looks like:
      ## method_name  run  train_ind  valid_ind  test_ind
      ##   CP_OLS       1  <dbl [ ]>  <dbl [ ]>  <dbl [ ]>
      ##   ...
      ## For each such row, we …
      pmap(\(method_name, run, train_ind, valid_ind, test_ind) {
        list(airport = key[[1, "airport"]], # … save the key, 
          horizon = key[[1, "horizon"]],
          method_name = method_name, # … the method
          run = run, # … and the run (∈ {1, …, 5})
          ## and we run the analysis on the current subset of the data.
          ## We nest that analysis inside a list.
          analysis = list(run_analysis(.data, method_name, train_ind, valid_ind, test_ind)))
      },
      ## First, we bind together the results of a single group into a data.table
      .progress = TRUE) |> rbindlist()
    ## The result of `group_map' is a list of data.tables, one for each group.
  }) |> rbindlist() # We bind these together into one big data.table with columns `airport', `horizon', `method_name', `run', `analysis'

