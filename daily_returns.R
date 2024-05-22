## Preliminaries
set.seed(42)

## Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

library(tikzDevice)
options(tikzDefaultEngine="luatex")

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

data <- data[23:nrow(data), c("X", "Y")] |> mutate(test_flag = FALSE)

### Analysis -------------------------------------------------------------------

alpha <- 0.1

n.ho <- floor(nrow(data)*0.10)

## TODO <2024-05-21 Tue> `cqr' is missing.
## NOTE <2024-05-22 Wed> `1:6' is hard-coded.
analysis_tibble <- tibble(name = c("dcp-qr", "dcp-opt", "dcp-dr", "cp-ols", "cp-loc", "dcp-idr"),
  fn = list(dcp.qr, dcp.opt, dcp.dr, cp.ols, cp.loc, dcp.idr),
  results = map(1:6, ~ tibble(coverage = NULL, leng = NULL))
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
  data[ind.test, "test_flag"] <- TRUE

  analysis_tibble <- analysis_tibble |>
    mutate(results = {
      map2(fn, results, ~ bind_rows(.y, .x(data.train, data.valid, data.test, alpha))
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
  width = 418.2555 / 72.27) # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.
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
    width = 418.2555 / 72.27) # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.
ggplot(plot_tibble, aes(x = bin, y = leng, color = as.factor(name), group = name)) +
  geom_point(aes(shape = as.factor(name)), size = 3) +
  geom_line() +
  xlab("Bin") +
  ylab("Length") +
  labs(color = "Method", shape = "Method") +
  theme_minimal()
dev.off()


