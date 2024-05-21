################################################################################

## Preliminaries
set.seed(42)

## Packages
library(dplyr)
library(tibble)

## Methods
source("methods.R")
## TODO <2024-05-21 Tue> `cqr' is missing
## NODE <2024-05-21 Tue> I wanted to something clever with `deparse(substitute())', but creating a list(dcp.qr, …) does not save the names.
## So I hard-coded the names here and loop over `names(methods)' below
methods <- list(dcp.qr = dcp.qr,
  dcp.opt = dcp.opt,
  dcp.dr = dcp.dr,
  cp.ols = cp.ols,
  cp.loc = cp.loc,
  dcp.idr = dcp.idr)


################################################################################
### Data
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

################################################################################
### Analysis
################################################################################

alpha <- 0.1

n.ho <- floor(nrow(data)*0.10)

## We do five runs.
## Consider data as 10 consecutive blocks of size ≈ n/10
## First run:  `cp' uses blocks 1–5; test uses block 6.
## Second run: `cp' uses blocks 2–6; test uses block 7.
## …
## Fifth run:  `cp' uses blocks 5–9; test uses block 10.
results <- list()
for (r in 1:5) {
  ## Define training and holdout samples.
  ind.cp <- ((r - 1)*n.ho + 1):(floor(nrow(data) * 0.5) + (r - 1) * n.ho + 1)
  ind.test <- (max(ind.cp)+1):(max(ind.cp)+n.ho)

  print(c(min(ind.cp), max(ind.cp)))
  print(c(min(ind.test), max(ind.test)))

  data.train <- train.valid.split(data[ind.cp, ])[["data.train"]]
  data.valid <- train.valid.split(data[ind.cp, ])[["data.valid"]]
  data.test <- data[ind.test, ]

  for (m in names(methods)) {
    ## Call the method.
    local.res <- methods[[m]](data.train, data.valid, data.test, alpha)

    ## Obtain previous results.
    local.coverage <- results[[m]][["coverage"]]
    local.leng <- results[[m]][["leng"]]

    results[[m]][["coverage"]] <- c(local.coverage, local.res$coverage)
    results[[m]][["leng"]] <- c(local.leng, local.res$leng)
  }
}

print("Mean coverage:")
## TODO <2024-05-21 Tue> Pretty print
print(lapply(results), \(m) mean(m[["coverage"]]))
print("Mean interval length:")
print(lapply(results), \(m) mean(m[["leng"]]))

################################################################################
### Plotting
################################################################################
