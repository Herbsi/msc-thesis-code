#!/usr/bin/env Rscript

options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

library(data.table)
library(ggplot2)
library(tidyverse, warn.conflicts = FALSE)

source("dcp.R")
source("lib.R")
source("plot-lib.R")

### Simulation -----------------------------------------------------------------

set.seed(42)

filename <- file.path("results", "motivating-example.rds")

x_test <- 5
cols <- c("qhatCC", "qhatL", "qhatU", "dcpCC", "dcpL", "dcpU")

if(file.exists(filename)) {
  result <- readRDS(filename)
} else {
  runs <- 512
  n <- 1024
  n_test <- 2048

  start <- Sys.time()
  result <- mclapply(1:runs, FUN = \(x) {
    message(str_c("Run", x, sep = " "))
    dtTrain <- data.table(X = runif(n, 0, 10))[, Y := rmodel(n, "S", X)][]
    dtValid <- data.table(X = runif(n, 0, 10))[, Y := rmodel(n, "S", X)][]
    dtTest  <- data.table(
      X = c(rep(x_test, times = n_test), runif(n_test, 0, 10)),
      type = c(rep("conditional", times = n_test), rep("unconditional", times = n_test)))[
      , Y := rmodel(2 * n_test, "S", X)][]
    dtTrain[, `:=`(X = X + runif(n, -1e-6, 1e-6), Y = Y + runif(n, -1e-6, 1e-6))]

    fitIDR <- idr(c(dtTrain$Y, dtValid$Y), rbindlist(list(dtTrain, dtValid))[, c("X")])
    dtTest[, c("qhatCC05", "qhatCC95") := as.data.table(qpred(predict(fitIDR, .SD) , c(0.05, 0.95)))][
    , qhatCC := (qhatCC05 <= Y) & (Y <= qhatCC95)][
    , `:=`(qhatCC05 = NULL, qhatCC95 = NULL)]

    dcpIDR <- dcp("IDR", Y ~ X, dtTrain, dtValid, copy(dtTest))
    dtTest[, dcpCC := dcpIDR$conditional_coverage]
    dtTest[type == "conditional" & qhatCC, let(qhatL = min(Y), qhatU = max(Y))]
    dtTest[type == "conditional" & dcpCC, let(dcpL = min(Y), dcpU = max(Y))]
    dtTest[, lapply(.SD, mean, na.rm = TRUE), by = type, .SDcols = cols]
  }, mc.cores = detectCores() - 1) |> rbindlist()

  elapsed <- difftime(Sys.time(), start, units = "secs")
  writeLines(str_c("Time:", elapsed, sep = " "))

  saveRDS(result, file = filename, compress = FALSE)
}


### Plotting -------------------------------------------------------------------

## Unconditional coverage
## Coverage
print(result[, lapply(.SD, mean), by = type, .SDcols = c("qhatCC", "dcpCC")])

resultForPlot <- result |>
  pivot_longer(
    cols = c(qhatCC, dcpCC),
    names_to = c("method"),
    names_pattern = "(qhat|dcp)CC",
    values_to = "value"
  ) |>
  group_by(type, method) |>
  mutate(mean = mean(value), sd = sd(value)) |>
  mutate(
    plot = factor("#", levels = c("Density", "#")),
    type = factor(type, levels = c("unconditional", "conditional")),
    method = case_match(method,
      "qhat" ~ "idr",
      "dcp" ~ "dcp-idr" # NOTE 2024-09-01 Hard-coded \dcp{\abb{idr}} = dcp-idr here.
    ) |>
    factor(levels = c("idr", "dcp-idr"))
  )

ggplot(resultForPlot) +
  facet_grid(rows = vars(type), scales = "free_y") +
  geom_histogram(
    mapping = aes(value, fill = method),
    binwidth = 0.005,
    alpha = 0.3,
  ) +
  geom_vline(
    aes(xintercept = mean, colour = method),
  ) +
  geom_vline(
    aes(xintercept = mean + sd, colour = method),
    linetype = 2
  ) +
  geom_vline(
    aes(xintercept = mean - sd, colour = method),
    linetype = 2
  ) +
  scale_dcp(
    breaks = unique(resultForPlot$method),
    limits = levels(resultForPlot$method)
  ) +
  labs(
    x = "Coverage", # NOTE 2024-09-01 U+01D44C and U+01D44B
    y = "#",
    colour = "",
    fill = ""
  ) +
  theme_dcp()
save_plot("motivatingExample.pdf")
