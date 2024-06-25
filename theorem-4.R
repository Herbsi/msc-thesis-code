source("lib.R")

results_tibble_thm4 <- run_experiment("theorem-4",
  model_name = c("AR(1)", "S1", "S1_2", "S1_3"),
  method_name = c("QR", "QR*", "IDR", "IDR*", "CP_OLS", "CP_LOC"),
  runs = 5,
  n = 2^(5:7))

## We have two results:
## - Asymptotic coverage
## - Minimal length

## Under the specified assumptions, we should observe
## 1. the correct coverage for the shape adjustment
## 2. a shorter interval than the not-adjusted method

## Lacking certain assumptions, we don't know what happens.
