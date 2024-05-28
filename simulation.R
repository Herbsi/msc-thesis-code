## Packages
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

library(tikzDevice)
options(tikzDefaultEngine="luatex")

source("lib.R")


## Setup -----------------------------------------------------------------------
n_train <- 5000
n_valid <- 10000
n_test <- 2000
n <- n_train + n_valid + n_test
ind_train <- 1:n_train
ind_valid <- (n_train+1):(n_train+n_valid)
ind_test <- (n_train+n_valid+1):n

split <- function(data) {
  list(train = data[ind_train, , drop = FALSE],
    valid = data[ind_valid, , drop = FALSE],
    test = data[ind_test, , drop = FALSE]
  )
}

model <- function(type) {
  force(type)
  switch(type,
    "D" = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) + 10 * (x >= 5),
    "P" = function(x) rpois(length(x), pmin(pmax(x, 1), 6)),
    "NI" = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 *(x > 7),
    "S" = function(x) rgamma(length(x), shape = sqrt(x), scale = pmin(pmax(x, 1), 6))
  )
}

type <- "S"

## Generate data
set.seed(42)
data_tibble <- tibble(X = runif(n, 0, 10),
  Y = model(type)(X)
) |>  
  mutate( # Add noise to training data.
    X = X + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train)),
    Y = Y + c(runif(n_train, -1e-6, 1e-6), rep(0, n - n_train))
  )

## Analysis -------------------------------------------------------------------

analysis_tibble <- data_tibble |>
  analyse(split, sig = 0.1) |>
  evaluate()


## Plotting --------------------------------------------------------------------
## Bin analysis-data into quantiles based on test data
X.test <- data_tibble[ind_test, ] |> pull(X)
binned_tibble <- analysis_tibble |>
  mutate(results.binned = map(results, ~ bin(.x, X.test, 20) |> filter(bin < 19))) |>
  mutate(averages = map(results.binned, ~ summarise(.x, coverage = mean(coverage), leng = mean(leng))), .keep = "unused")

plot_tibble <- analysis_tibble[, c("name", "averages")] |>
  mutate(name = str_c("\\textsc{", name, "}")) |>
  unnest(cols = c("name", "averages"))

ggplot(plot_tibble, aes(x = bin, y = coverage, color = as.factor(name), group = name)) +
  geom_point(aes(shape = as.factor(name)), size = 3) +
  geom_line() +
  xlab("Bin") +
  ylab("Coverage") +
  labs(color = "Method", shape = "Method") +
  theme_minimal()

ggplot(plot_tibble, aes(x = bin, y = leng, color = as.factor(name), group = name)) +
  geom_point(aes(shape = as.factor(name)), size = 3) +
  geom_line() +
  xlab("Bin") +
  ylab("Length") +
  labs(color = "Method", shape = "Method") +
  theme_minimal()

