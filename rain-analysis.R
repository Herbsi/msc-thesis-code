library(dplyr, warn.conflicts = FALSE)
library(ggh4x)
library(lubridate, warn.conflicts = FALSE) # as_date
library(stringr) # str_c, str_sub
library(tidyr)

source("plot-lib.R")
source("table-lib.R")

### Load results ---------------------------------------------------------------

## TODO 2024-08-23 Turn these into command-line arguments
resultdcp <- readRDS("results/precipitation/20240819135432/resultdcp.rds")
resultziegel <- readRDS("results/precipitation/20240819135432/resultziegel.rds")


## Create joined result

resultdcp[, approach := "dcp"]
resultziegel[, approach := "ziegel"]
results <- rbindlist(list(resultdcp, resultziegel))


### Unconditional table --------------------------------------------------------

results |>
  select(airport, horizon, method, approach, coverage, leng) |>
  mutate(
    coverage = round(100 * coverage, 1),
    leng = round(leng, 3)
  ) |>
  dcast(method + horizon ~ airport + approach, value.var = c("coverage", "leng")) |>
  arrange(method, horizon) |>
  mutate(method = renameForCSV(method)) |>
  writeTable("rainUncond.csv")


### Unconditional plot ---------------------------------------------------------
results |>
  ## Preparation
  rename(Coverage = coverage, Length = leng) |>
  pivot_longer(cols = c(Coverage, Length),
    names_to = "metric", values_to = "value") |>
  mutate(
    airport = renameForPlot(airport),
    method = renameForPlot(method),
    approach = renameForPlot(approach)
  ) |>
  ## Plotting
  ggplot() +
  geom_point(aes(x = horizon, y = value, color = method, group = method)) +
  facet_nested(metric ~ airport + approach, scales = "free_y") +
  labs(
    x = "Horizon",
    y = "",
    shape = "",
    color = "Method",) +
  theme_dcp()
## Save plot
savePlot("rainUncond.pdf")


### CCMSE Table ----------------------------------------------------------------
results |>
  select(
    airport,
    horizon,
    method,
    approach,
    conditional_coverage_mse_full,
    conditional_coverage_mse_hres
  ) |>
  mutate(
    conditional_coverage_mse_full = round(conditional_coverage_mse_full, 3),
    conditional_coverage_mse_hres = round(conditional_coverage_mse_hres, 3)
  ) |>
  dcast(
    method + horizon ~ airport + approach,
    value.var = c("conditional_coverage_mse_full", "conditional_coverage_mse_hres")
  ) |>
  arrange(method, horizon) |>
  mutate(method = renameForCSV(method)) |>
  writeTable("rainCCMSE.csv")


### CCMSE Graph ----------------------------------------------------------------
resultziegel |>
  ## Preparation
  pivot_longer(
    cols = c(conditional_coverage_mse_full, conditional_coverage_mse_hres),
    names_to = "mse_type", values_to = "value"
  ) |>
  mutate(
    airport = renameForPlot(airport),
    mse_type = renameForPlot(mse_type),
    method = renameForPlot(method)
  ) |>
  ## Plotting
  ggplot() +
  geom_point(aes(
    x = method,
    y = value,
    shape = mse_type
  )) +
  facet_grid(rows = vars(airport), cols = vars(horizon), scales = "fixed") +
  labs(
    x = "Horizon",
    y = "ccmse",
    shape = "",
    ) +
  theme_dcp() +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, family = familyCaps),
    axis.title.y = element_text(family = familyCaps))
## Save plot
savePlot("rainCCMSE.pdf")


### Coverage and Length vs Date plot -------------------------------------------
## For Full and HRES
## TODO 2024-08-20 Maybe turn into two plots, as the length plot, of course, does not have different lines for full and hres.
resultziegel |>
  ## Preparation
  ## Only use Zurich with horizon 2
  filter(horizon == 2 & airport == "zrh") |>
  ## Unnest coverage and length and rename; this could be cleaner
  select(method, conditional_coverage, conditional_leng) |>
  unnest(c(conditional_coverage, conditional_leng), names_sep = ".") |>
  select(!conditional_leng.date) |>
  rename(date = conditional_coverage.date) |>
  ## Summarise by month, meaning coverage and length
  group_by(
    method, year = year(date), month = month(date)) |>
  summarise(
    full_cov = mean(conditional_coverage.conditional_coverage_full),
    hres_cov = mean(conditional_coverage.conditional_coverage_hres),
    full_leng = mean(conditional_leng.conditional_leng, na.rm = TRUE),
    .groups = "drop",
  ) |>
  ## Pivot longer so we can plot it
  pivot_longer(
    cols = c(full_cov, hres_cov, full_leng),
    names_to = "type", values_to = "value") |>
  ## Rename columns
  mutate(
    metric = as.factor(if_else(str_sub(type, 6, -1) == "cov", "Coverage", "Length")),
    type = as.factor(if_else(str_sub(type, 1, 4) == "full", "full", "hres")),
    date = as_date(str_c(year, month, "01", sep = "-")),
    coverage_type = as.factor(type),
    method = renameForPlot(method)) |>
  ## Plot
  ggplot() +
  geom_line(
    aes(x = date, y = value, color = method, linetype = type)
  ) +
  facet_grid(rows = c("metric"), scales = "free_y") +
  labs(
    x = "Date",
    y = "",
    color = "Method",
    linetype = "",
  ) +
  theme_dcp()
## Save plot
savePlot("rainDateZRH2.pdf")

## TODO 2024-08-23 Analyse result_hres
