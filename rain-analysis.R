library(dplyr, warn.conflicts = FALSE)
library(ggh4x)
library(lubridate, warn.conflicts = FALSE) # as_date
library(stringr) # str_c
library(tidyr)

source("plot-lib.R")
source("table-lib.R")

### Load results ---------------------------------------------------------------

## TODO 2024-08-23 Turn these into command-line arguments
results_dcp_cw <- readRDS("results/precipitation/20240825104303/results_dcp_cw.rds")
results_dcp_icx <- readRDS("results/precipitation/20240825104303/results_dcp_icx.rds")
results_ziegel_cw <- readRDS("results/precipitation/20240825104303/results_ziegel_cw.rds")
results_ziegel_icx <- readRDS("results/precipitation/20240825104303/results_ziegel_icx.rds")


## Create joined result

results_dcp_cw[, `:=`(approach = "dcp", variant = "cw")]
results_dcp_icx[, `:=`(approach = "dcp", variant = "icx")]
results_ziegel_cw[, `:=`(approach = "ziegel", variant = "cw")]
results_ziegel_icx[, `:=`(approach = "ziegel", variant = "icx")]
results <- rbindlist(list(results_dcp_cw, results_dcp_icx, results_ziegel_cw, results_ziegel_icx))


### Unconditional table --------------------------------------------------------

## FIXME 2024-08-25 Adapt table to new results format.
## results |>
##   select(airport, horizon, method, approach, coverage, leng) |>
##   mutate(
##     coverage = round(100 * coverage, 1),
##     leng = round(leng, 3)
##   ) |>
##   dcast(method + horizon ~ airport + approach, value.var = c("coverage", "leng")) |>
##   arrange(method, horizon) |>
##   mutate(method = renameForCSV(method)) |>
##   writeTable("rainUncond.csv")


### Unconditional plot ---------------------------------------------------------
dt <- results |>
  ## Preparation
  rename(Coverage = coverage, Length = leng) |>
  pivot_longer(cols = c(Coverage, Length),
    names_to = "metric", values_to = "value") |>
  mutate(
    airport = renameForPlot(airport),
    method = renameForPlot(method),
    approach = renameForPlot(approach),
    variant = renameForPlot(variant)
  ) |>
  mutate(method = factor(method, levels = methodLevels))

## Plot
ggplot(dt) +
  geom_point(aes(x = horizon, y = value, color = method, group = method, shape = variant)) +
  facet_nested(metric ~ airport + approach, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", limits = methodLevels, breaks = unique(dt$method)) +
  labs(
    x = "Horizon",
    y = "",
    shape = "Variant",
    color = "Method",) +
  theme_dcp() +
  theme(
    axis.title.x = element_text(family = family),
    axis.text.x = element_text(family = family)
  )

## Save plot
savePlot("rainUncond.pdf")


### CCMSE Table ----------------------------------------------------------------
## FIXME 2024-08-25 Adapt table to new results format.
## results |>
##   select(
##     airport,
##     horizon,
##     method,
##     approach,
##     conditional_coverage_mse_full,
##     conditional_coverage_mse_hres
##   ) |>
##   mutate(
##     conditional_coverage_mse_full = round(conditional_coverage_mse_full, 3),
##     conditional_coverage_mse_hres = round(conditional_coverage_mse_hres, 3)
##   ) |>
##   dcast(
##     method + horizon ~ airport + approach,
##     value.var = c("conditional_coverage_mse_full", "conditional_coverage_mse_hres")
##   ) |>
##   arrange(method, horizon) |>
##   mutate(method = renameForCSV(method)) |>
##   writeTable("rainCCMSE.csv")


### CCMSE Graph ----------------------------------------------------------------
dt <- results |>
  ## Preparation
  filter(approach == "ziegel") |>
  mutate(
    airport = renameForPlot(airport),
    method = factor(renameForPlot(method), methodLevels)
  ) |>
  mutate(method = factor(method, levels = methodLevels))

## Plot
ggplot(dt) +
  geom_point(aes(
    x = method,
    y = ccmse,
    shape = variant,
  )) +
  facet_grid(rows = vars(airport), cols = vars(horizon), scales = "fixed") +
  labs(
    x = "Method",
    y = "ccmse",
    shape = "Variant",
    ) +
  theme_dcp() +
  theme(
    axis.text.x = element_text(size = 7, angle = -45, hjust = 0, vjust = 1, family = familyCaps),
    axis.title.x = element_text(family = family),
    axis.title.y = element_text(family = familyCaps))
## Save plot
savePlot("rainCCMSE.pdf")


### Coverage and Length vs Date plot -------------------------------------------
dt <- results |>
  ## Preparation
  ## Only use Zurich with horizon 2
  filter(approach == "ziegel" & horizon == 2 & airport == "zrh") |>
  ## Unnest coverage and length and rename; this could be cleaner
  select(method, variant, conditional) |>
  unnest(conditional) |>
  ## Summarise by month, meaning coverage and length
  group_by(
    method, variant, year = year(date), month = month(date)) |>
  summarise(
    Coverage = mean(coverage),
    Length = mean(leng, na.rm = TRUE),
    .groups = "drop",
  ) |>
  ## Pivot longer so we can plot it
  pivot_longer(
    cols = c(Coverage, Length),
    names_to = "metric", values_to = "value") |>
  mutate(
    date = as_date(str_c(year, month, "01", sep = "-")),
    method = renameForPlot(method)
  ) |>
  mutate(method = factor(method, levels = methodLevels))

## Plot
ggplot(dt) +
  geom_line(
    aes(x = date, y = value, color = method, linetype = variant)
  ) +
  facet_grid(rows = c("metric"), scales = "free_y") +
  scale_colour_brewer(palette = "Set1", limits = methodLevels, breaks = unique(dt$method)) +
  labs(
    x = "Date",
    y = "",
    color = "Method",
    linetype = "Variant",
  ) +
  theme_dcp() +
  theme(
    axis.title.x = element_text(family = family),
    axis.text.x = element_text(family = family),
    )
## Save plot
savePlot("rainDateZRH2.pdf")
