library(ggplot2)
library(tidyverse)

source("dcp.R")
source("lib.R")
source("plot-lib.R")


### Simulation -----------------------------------------------------------------

set.seed(42)

filename <- file.path("results", "motivating-example.RDS")

xTest <- 5
cols <- c("qCC", "qhatL", "qhatU", "dcpCC", "dcpL", "dcpU")

if(file.exists(filename)) {
  message("Loading existing result.")

  result <- readRDS(filename)
} else {
  message("Running new simulation.")

  runs <- 256
  n <- 512
  nTest <- 2048

  start <- Sys.time()
  result <- mclapply(1:runs, FUN = \(x) {
    message(str_c("Run", x, sep = " "))
    dtTrain <- data.table(X = runif(n, 0, 10))[, Y := rmodel(n, "S", X)][]
    dtValid <- data.table(X = runif(n, 0, 10))[, Y := rmodel(n, "S", X)][]
    dtTest  <- data.table(
      X = c(rep(xTest, times = nTest), runif(nTest, 0, 10)),
      type = c(rep("conditional", times = nTest), rep("unconditional", times = nTest)))[
      , Y := rmodel(2 * nTest, "S", X)][]
    dtTrain[, `:=`(X = X + runif(n, -1e-6, 1e-6), Y = Y + runif(n, -1e-6, 1e-6))]

    fitIDR <- idr(c(dtTrain$Y, dtValid$Y), rbindlist(list(dtTrain, dtValid))[, c("X")])
    dtTest[, c("qCC05", "qCC95") := as.data.table(qpred(predict(fitIDR, .SD) , c(0.05, 0.95)))][
    , qCC := (qCC05 <= Y) & (Y <= qCC95)][
    , `:=`(qCC05 = NULL, qCC95 = NULL)]

    dcpIDR <- dcp("IDR", Y ~ X, dtTrain, dtValid, copy(dtTest))
    dtTest[, dcpCC := dcpIDR$conditional_coverage]
    dtTest[type == "conditional" & qCC, let(qhatL = min(Y), qhatU = max(Y))]
    dtTest[type == "conditional" & dcpCC, let(dcpL = min(Y), dcpU = max(Y))]
    dtTest[, lapply(.SD, mean, na.rm = TRUE), by = type, .SDcols = cols]
  }, mc.cores = detectCores() - 1) |> rbindlist()

  elapsed <- difftime(Sys.time(), start, units = "secs")
  writeLines(str_c("Time:", elapsed, sep = " "))

  saveRDS(result, file = filename, compress = FALSE)
}


### Plotting -------------------------------------------------------------------

05 <- qgamma(0.05, shape = sqrt(xTest), scale = pmin(pmax(xTest, 1), 6))
q95 <- qgamma(0.95, shape = sqrt(xTest), scale = pmin(pmax(xTest, 1), 6))

message("Plotting results.")

resultForPlot <- result[type == "conditional", ] |>
  pivot_longer(
    cols = c(qhatL, dcpL, qhatU, dcpU),
    names_to = c("method", "quantile"),
    names_pattern = "(qhat|dcp)([LU])",
    values_to = "value"
  ) |>
  mutate(
    plot = factor("#", levels = c("Density", "#")),
    method = case_match(method,
      "qhat" ~ "idr",
      "dcp" ~ "dcp-idr" # NOTE 2024-09-01 Hard-coded \dcp{\abb{idr}} = dcp-idr here.
    ) |>
    factor(levels = c("idr", "dcp-idr"))
  )

dataForPDF <- tibble(
  plot = factor("Density", levels = c("Density", "#")),
  X = xTest,
  Y = seq(0, 40, length.out = 1000),
  f = dgamma(Y, shape = sqrt(X), scale = pmin(pmax(X, 1), 6))
)


dataForPlot <- bind_rows(dataForPDF, resultForPlot)

ggplot(dataForPlot) +
  facet_grid(rows = vars(plot), scales = "free_y") +
  geom_line(
    mapping = aes(x = Y, y = f),
  ) +
  geom_histogram(
    mapping = aes(value, fill = method),
    filter(dataForPlot, quantile == "L"),
    binwidth = 0.3,
    alpha = 0.3,
  ) +
  geom_histogram(
    mapping = aes(value, fill = method),
    dataForPlot |> filter(quantile == "U") |> mutate(value = pmin(40, value)),
    binwidth = 0.3,
    alpha = 0.3,
  ) +
  ## FIXME 2024-05-09 This code is rather redundant, but I haven’t bothered to find a better way yet.
  geom_vline(
    aes(xintercept = q05),
    dataForPlot |> filter(plot == "#")
  ) +
  geom_vline(
    aes(xintercept = q95),
    dataForPlot |> filter(plot == "#")
  ) +
  geom_vline(aes(
    xintercept = result[, mean(qhatL, na.rm = TRUE)],
    colour = factor("idr", levels = c("idr", "dcp-idr"))
  ),
  dataForPlot |> filter(plot == "#")
  ) +
  geom_vline(aes(
    xintercept = result[, mean(qhatU, na.rm = TRUE)],
    colour = factor("idr", levels = c("idr", "dcp-idr"))
  ),
  dataForPlot |> filter(plot == "#")
  ) +
  geom_vline(aes(
    xintercept = result[, mean(dcpL, na.rm = TRUE)],
    colour = factor("dcp-idr", levels = c("idr", "dcp-idr"))
  ),
  dataForPlot |> filter(plot == "#")
  ) +
  geom_vline(aes(
    xintercept = result[, mean(dcpU, na.rm = TRUE)],
    colour = factor("dcp-idr", levels = c("idr", "dcp-idr"))
  ),
  dataForPlot |> filter(plot == "#")
  ) +
  scale_dcp(
    breaks = unique(resultForPlot$method),
    limits = levels(resultForPlot$method)
  ) +
  labs(
    x = str_c("𝑌 | 𝑋 = ", xTest), # NOTE 2024-09-01 U+01D44C and U+01D44B
    y = "",
    colour = "",
    fill = ""
  ) +
  theme_dcp()
savePlot("motivatingExample.pdf")
