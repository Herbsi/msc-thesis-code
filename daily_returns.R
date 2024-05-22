## Preliminaries
set.seed(42)

## Packages
library(dplyr)
library(tibble)

source("lib.R")
## Methods
source("methods.R")


### Data -----------------------------------------------------------------------
## Source: Kenneth R. French data library
## Accessed: August 17, 2021
## URL: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

data <- as_tibble(read.csv("data_us_08172021.CSV")) |> mutate(Y = MktRF + RF, X = NA)

## TODO <2024-05-21 Tue> Rewrite this using `tidyverse'
for (i in 23:nrow(data)) {
  ## DCP-paper calls this “lagged realised volatility”
  data[i, "X"] <- sqrt(sum(data[(i-22):(i-1), "Y"]^2))
}

data <- data[23:nrow(data), c("X", "Y")]

### Analysis -------------------------------------------------------------------

alpha <- 0.1

n.ho <- floor(nrow(data)*0.10)

## TODO <2024-05-21 Tue> `cqr' is missing.
## NOTE <2024-05-22 Wed> `1:6' is hard-coded.
results <- tibble(name = c("dcp.qr", "dcp.opt", "dcp.dr", "cp.ols", "cp.loc", "dcp.idr"),
  fn = list(dcp.qr, dcp.opt, dcp.dr, cp.ols, cp.loc, dcp.idr),
  result = map(1:6, ~ tibble(coverage = NULL, leng = NULL))
)

## We do five runs.
## Consider data as 10 consecutive blocks of size ≈ n/10
## First run:  `cp' uses blocks 1–5; test uses block 6.
## Second run: `cp' uses blocks 2–6; test uses block 7.
## …
## Fifth run:  `cp' uses blocks 5–9; test uses block 10.
for (r in 1:5) {
  ## Define training and holdout samples.
  ind.cp <- ((r - 1)*n.ho + 1):(floor(nrow(data) * 0.5) + (r - 1) * n.ho + 1)
  ind.test <- (max(ind.cp)+1):(max(ind.cp)+n.ho)

  print(c(min(ind.cp), max(ind.cp)))
  print(c(min(ind.test), max(ind.test)))

  data.train <- train_valid_split(data[ind.cp, ])[["data.train"]]
  data.valid <- train_valid_split(data[ind.cp, ])[["data.valid"]]
  data.test <- data[ind.test, ]

  results <- results |>
    mutate(result = {
      ## NOTE <2024-05-21 Tue> `fn' is a one-element list with a function for some reason; extract it with `[[1]]'
      map(result, ~ bind_rows(.x, fn[[1]](data.train, data.valid, data.test, alpha))
      )
    })
}

### Plotting -------------------------------------------------------------------

