library(dplyr, warn.conflicts=FALSE)
library(lubridate, warn.conflicts=FALSE)
library(tibble)

source("dcp.R")

load("data/precipData_caseStudy.rda")
precipitation <- as_tibble(precipData_caseStudy)
incomplete_days <- precipitation |>
  group_by(date) |>
  summarise(count = n()) |>
  filter(count < 20) |>
  pull(date)
precipitation <- precipitation |>
  filter(!(date %in% incomplete_days))

## TODO 2024-07-21
## Follow DCP paper:
## - For each method
## - Fit on 50% predict on next 10%
## - Do this 5 times
## Check

## TODO 2024-07-21 I don't understand how `horizon' works.
## Neither of the following relations hold:
## - (`airport', `date') => `obs', ie `airport' and `date' determine `obs'
## - (`airport', `date' + `horizon') => `obs', ie `aiport' and the same ‘future date’ determine `obs' on that day 

precipitation |>
  group_by(airport, date, obs) |>
  count()

precipitation |>
  mutate(horizon_date = date + as.numeric(horizon)) |>
  group_by(airport, horizon_date, obs) |>
  count()

