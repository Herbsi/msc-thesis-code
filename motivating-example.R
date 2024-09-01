library(ggplot2)
library(tidyverse)

source("dcp.R")
source("lib.R")
source("plot-lib.R")


### Simulation -----------------------------------------------------------------

filename <- file.path("results", "motivating-example.RDS")

xTest <- 7.5
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

message("Plotting results.")
result <- result[, lapply(.SD, mean), by = type, .SDcols = cols]
resultForPlot <- result[type == "conditional", ] |>
  mutate(
    qL = qgamma(0.05, shape = sqrt(xTest), scale = pmin(pmax(xTest, 1), 6)),
    qU = qgamma(0.95, shape = sqrt(xTest), scale = pmin(pmax(xTest, 1), 6))
  ) |>
  pivot_longer(
    cols = c(qL, qhatL, dcpL, qU, qhatU, dcpU),
    names_to = c("method", "quantile"),
    names_pattern = "(q|qhat|dcp)([LU])",
    values_to = "value")

dataForPDF <- tibble(
  X = xTest,
  Y = seq(0, 45, length.out = 1000),
  f = dgamma(Y, shape = sqrt(X), scale = pmin(pmax(X, 1), 6))
)

## TODO 2024-09-01 Make this plot prettier.
ggplot(dataForPDF, aes(x = Y, y = f)) +
  geom_line() +
  geom_vline(data = resultForPlot, mapping = aes(xintercept = value, linetype = method)) +
  labs(
    x = str_c("𝑌"), # NOTE 2024-09-01 This is U+01D44C
    y = "Density") +
  theme_dcp()
savePlot("motivatingExample.pdf")
