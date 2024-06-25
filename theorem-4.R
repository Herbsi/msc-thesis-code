source("lib.R")

results_tibble <- theorem_4(runs = 5, n = 2^(5:7))

## We have two results:
## - Asymptotic coverage
## - Minimal length

## Under the specified assumptions, we should observe
## 1. the correct coverage for the shape adjustment
## 2. a shorter interval than the not-adjusted method

## Lacking certain assumptions, we don't know what happens.
