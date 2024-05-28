## Preliminaries
set.seed(42)

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
## DCP Methods
source("dcp.R")


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

data <- data[23:nrow(data), c("X", "Y")] |> mutate(test_flag = FALSE)

### Analysis -------------------------------------------------------------------

alpha <- 0.1

n_ho <- floor(nrow(data)*0.10)

## TODO <2024-05-21 Tue> `cqr' is missing.
## TODO <2024-05-24 Fri> `qr-*' is missing 
analysis_tibble <- tibble(name = c("dcp-qr",  "dcp-dr", "dcp-idr", "cp-ols", "cp-loc"),
  fn = list(dcp_qr, dcp_dr, dcp_idr, dcp_cp_ols, dcp_cp_loc),
  results = map(seq_along(name), ~ tibble(coverage = NULL, leng = NULL))
)

## We do five runs.
## Consider data as 10 consecutive blocks of size ≈ n/10
## First run:  `cp' uses blocks 1–5; test uses block 6.
## Second run: `cp' uses blocks 2–6; test uses block 7.
## …
## Fifth run:  `cp' uses blocks 5–9; test uses block 10.
for (r in 1:5) {
  ## Define training, calibration, and holdout samples.
  ind_train <- ((r - 1)*n_ho + 1):(floor(nrow(data) * .25) + (r - 1) * n_ho + 1)
  ind_valid <- (max(ind_train)+1):(floor(nrow(data) * .50) + (r - 1) * n_ho + 1)
  ind_test <- (max(ind_valid)+1):(max(ind_valid)+n_ho)

  split <- function(data) {
    list(train = data[ind_train, , drop = FALSE],
      valid = data[ind_valid, , drop = FALSE],
      test = data[ind_test, , drop = FALSE]
    )
  }
  
  print(c(min(ind_train), max(ind_train)))
  print(c(min(ind_valid), max(ind_valid)))
  print(c(min(ind_test), max(ind_test)))

  data[ind_test, "test_flag"] <- TRUE

  analysis_tibble <- analysis_tibble |>
    mutate(results = {
      map2(fn, results, ~ bind_rows(.y, .x(Y ~ X, data, split, alpha))
      )
    })
}

### Plotting -------------------------------------------------------------------

## Bin analysis-data into quantiles based on test data
X.test <- data |> filter(test_flag == TRUE) |> pull(X)
## TODO <2024-05-22 Wed> Currently, the final bin is odd.
analysis_tibble <- analysis_tibble |>
  mutate(results.binned = map(results, ~ bin(.x, X.test, 20) |> filter(bin < 19))) |>
  mutate(averages = map(results.binned, ~ summarise(.x, coverage = mean(coverage), leng = mean(leng))), .keep = "unused")

plot_tibble <- analysis_tibble[, c("name", "averages")] |>
  mutate(name = str_c("\\textsc{", name, "}")) |>
  unnest(cols = c("name", "averages"))

## Plot coverage
## BEGIN COPY-PASTE
tikz(file = "../tex/images/tikz/coverage.tex",
  width = 418.2555 / 72.27 # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.
) 
print(
  ggplot(plot_tibble, aes(x = bin, y = coverage, color = as.factor(name), group = name)) +
    geom_point(aes(shape = as.factor(name)), size = 3) +
    geom_line() +
    xlab("Bin") +
    ylab("Coverage") +
    labs(color = "Method", shape = "Method") +
    theme_minimal()
)
dev.off()
## END COPY-PASTE

tikz(file = "../tex/images/tikz/length.tex",
  width = 418.2555 / 72.27 # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.
) 
ggplot(plot_tibble, aes(x = bin, y = leng, color = as.factor(name), group = name)) +
  geom_point(aes(shape = as.factor(name)), size = 3) +
  geom_line() +
  xlab("Bin") +
  ylab("Length") +
  labs(color = "Method", shape = "Method") +
  theme_minimal()
dev.off()


