#!/usr/bin/env Rscript

library(dplyr, warn.conflicts = FALSE)
library(ggh4x)
library(lubridate, warn.conflicts = FALSE) # as_date
library(purrr)
library(stringr) # str_c
library(tidyr)

source("plot-lib.R")
source("table-lib.R")

alpha_sig <- 0.1

### Load results ---------------------------------------------------------------

dir <- file.path("results", "precipitation", "20240908111824")

results_dcp_cw <- readRDS(file.path(dir, "results_dcp_cw.rds"))
results_dcp_icx <- readRDS(file.path(dir, "results_dcp_icx.rds"))
results_ziegel_cw <- readRDS(file.path(dir, "results_ziegel_cw.rds"))
results_ziegel_icx <- readRDS(file.path(dir, "results_ziegel_icx.rds"))

## Create joined result

results_dcp_cw[, `:=`(approach = "dcp", variant = "cw")]
results_dcp_icx[, `:=`(approach = "dcp", variant = "icx")]
results_ziegel_cw[, `:=`(approach = "ziegel", variant = "cw")]
results_ziegel_icx[, `:=`(approach = "ziegel", variant = "icx")]
results <- rbindlist(list(results_dcp_cw, results_dcp_icx, results_ziegel_cw, results_ziegel_icx))


### Unconditional plot ---------------------------------------------------------

dt <- results |>
  rename(Coverage = coverage, Length = leng) |>
  pivot_longer(
    cols = c(Coverage, Length),
    names_to = "metric",
    values_to = "value"
  ) |>
  prepare_for_plot(c("airport", "method", "approach", "variant"))

ggplot(dt) +
  geom_point(aes(
    x = horizon,
    y = value,
    color = method,
    group = method,
    shape = variant
  )) +
  facet_nested(metric ~ airport + approach, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", limits = method_levels, breaks = unique(dt$method)) +
  labs(
    x = "Horizon",
    y = "",
    shape = "Variant",
    color = "Method"
  ) +
  theme_dcp() +
  theme(
    axis.title.x = element_text(family = family),
    axis.text.x = element_text(family = family)
  )
save_plot("unconditional.pdf", sub_dir = "rain")


### CCMSE Graph ----------------------------------------------------------------

dt <- results |>
  filter(approach == "ziegel") |>
  mutate(ccmse = purrr::map_dbl(conditional,
    \(tibble) sqrt(mean((tibble$pred - (1 - alpha_sig))^2))
  )) |>
  prepare_for_plot(c("airport", "method"))

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
    axis.title.y = element_text(family = familyCaps)
  )
## Save plot
save_plot("ccmse.pdf", sub_dir = "rain")


### FSC Graph ------------------------------------------------------------------

dt <- results |>
  filter(approach == "ziegel") |>
  select(method, airport, horizon, variant, conditional) |>
  unnest(conditional) |>
  group_by(method, airport, horizon, variant, year = year(date), month = month(date)) |>
  summarise(fsc = mean(coverage)) |>
  group_by(method, airport, horizon, variant) |>
  summarise(fsc = min(fsc), .groups = "drop") |>
  prepare_for_plot(c("airport", "method", "variant"))

ggplot(dt) +
  geom_point(aes(
    x = method,
    y = fsc,
    shape = variant,
  )) +
  facet_grid(rows = vars(airport), cols = vars(horizon)) +
  labs(
    x = "Method",
    y = "fsc",
    shape = "Variant",
    ) +
  geom_hline(aes(yintercept = 0.9)) +
  scale_y_continuous(limit = c(0.3, 1)) +
  theme_dcp() +
  theme(
    axis.text.x = element_text(size = 7, angle = -45, hjust = 0, vjust = 1, family = familyCaps),
    axis.title.x = element_text(family = family),
    axis.title.y = element_text(family = familyCaps)
  )
save_plot("fsc.pdf", sub_dir = "rain")

### Coverage and Length vs Date plot -------------------------------------------

dt <- results |>
  ## Only use Zurich with horizon 2
  filter(approach == "ziegel" & horizon == 2 & airport == "zrh") |>
  ## Unnest coverage and length and rename; this could be cleaner
  select(method, variant, conditional) |>
  unnest(conditional) |>
  ## Summarise by month, meaning coverage and length
  group_by(method, variant, year = year(date), month = month(date)) |>
  summarise(
    Coverage = mean(coverage),
    Length = mean(leng, na.rm = TRUE),
    .groups = "drop",
  ) |>
  ## Pivot longer so we can plot it
  pivot_longer(
    cols = c(Coverage, Length),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(date = as_date(str_c(year, month, "01", sep = "-"))) |>
  prepare_for_plot(c("method", "variant"))

ggplot(dt) +
  geom_line(
    aes(x = date, y = value, color = method, linetype = variant),
    linewidth = 0.5
  ) +
  facet_grid(rows = c("metric"), scales = "free_y") +
  scale_dcp(breaks = unique(dt$method)) +
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
save_plot("conditional_ZRH2.pdf", sub_dir = "rain")
