#!/usr/bin/env Rscript

### Packages -------------------------------------------------------------------

options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

library(ggplot2)
library(scales, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

source("plot-lib.R")
source("table-lib.R")


### Load data ------------------------------------------------------------------

results3 <- readRDS(file.path("results", "theorem-3-euler", "20240909091803", "result070756.rds"))
results4 <- readRDS(file.path("results", "theorem-4-euler", "20240909091825", "result153438.rds"))

alpha_sig <- 0.1
n_values <- c(128, 1024, 16384)
model_values <- c("AR(NI)", "NI", "AR(P)", "S")


### Geoms ----------------------------------------------------------------------

geom_unconditional <- function(y) {
  list(
    geom_line(
      aes(x = n, y = {{ y }}, colour = method, group = method),
      linewidth = 0.5
    ),
    labs(
      x = "𝑛", # NOTE 2024-08-23 This is U+1D45B
      colour = "Method",
    ),
    scale_x_continuous(transform = "log2"),
    scale_y_log10(),
    theme_dcp()
  )
}


geom_conditional <- function(y, scales = "fixed") {
  list(
    geom_line(
      aes(x = X, y = {{ y }}, colour = method),
      linewidth = 0.5
    ),
    facet_grid(model ~ n, scales = scales),
    labs(
      x = "𝑋", # NOTE 2024-08-23 This is U+1D44B
      colour = "Method",
    ),
    theme_dcp(),
    theme(strip.text.x = element_text(family = familyMath))
  )
}


### Functions ------------------------------------------------------------------

predict.tbl_df <- function(object, newdata) {
  intercept <- object |> filter(term == "(Intercept)") |> pull(estimate)
  slope <- object |> filter(term == "X") |> pull(estimate)
  linear_predictor <- intercept + slope * newdata
  plogis(linear_predictor)
}


### Plotting functions --------------------------------------------------------- 

#### Unconditional – functions of `n'

plot_unconditional <- function(dt, metrics) {
  ## NOTE 2024-10-10 This is a bit hacky.  I hard-code the order of the metrics here by turning them into a factor.
  metric_levels <- c("Coverage", "Length", "fsc", "ccmse")
  
  dt <- dt |>
    pivot_longer(
      cols = all_of(metrics),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(metric = factor(
      case_match(metric,
        "coverage" ~ "Coverage",
        "leng" ~ "Length",
        .default = metric),
      levels = metric_levels))

  ggplot(dt) +
    facet_grid(rows = vars(metric), cols = vars(model), scales = "free_y") +
    geom_unconditional(y = value) +
    scale_dcp(breaks = unique(dt$method)) +
    theme_dcp() +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.x = element_text(family = familyCaps),
      strip.text.y = if (length(metrics) == 1) element_blank() else element_text()
    )
}


#### Conditional – functions of `X'

plot_conditional <- function(dt, metric, scales = "fixed") {
  dt <- dt |>
    unnest(conditional, names_sep = "_") |>
    mutate(X = map_dbl(conditional_cut, \(bin) {
      unlist(strsplit(gsub("[^0-9e.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    }))

  ggplot(dt) +
    geom_conditional(
      y = !!sym(paste0("conditional_", metric)),
      scales = scales
    ) +
    scale_dcp(breaks = unique(dt$method)) +
    theme_dcp() +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.x = element_text(family = familyMath),
      strip.text.y = element_text(family = familyCaps)
    )
}


plot_conditional_coverage_glm <- function(dt) {
  dt <- dt |>
    mutate(conditional_coverage_glm =
             ## What happens here is that, the column
             ## `conditional_coverage_glm' of `dt' is mutated to another
             ## column with the same name, but where each entry is now a
             ## tibble with two columns: `X' and `conditional_coverage_pred'.
             ## `X' is a fine grid in [0, 10]; `conditional_coverage_pred' is
             ## the GLM inside `dt$conditional_coverage_glm' evaluated along
             ## that grid.
             map(conditional_coverage_glm, \(tibble) {
               tibble(X = seq(0, 10, length.out = 1000),
                 conditional_coverage_pred = predict(tibble, X))
             })) |>
    ## To plot this evaluated GLM, we unnest the tibble.
    unnest(conditional_coverage_glm)

  ggplot(dt) +
    geom_conditional(y = conditional_coverage_pred) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Conditional coverage smoothed") +
    theme_dcp() +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.y = element_text(family = familyCaps)
    )
}


### Analysis -------------------------------------------------------------------

## Add quantities numbers for conditional coverage

results3 <- results3 |>
  mutate(
    fsc = map_dbl(conditional, \(dt) min(dt$coverage)),
    ccmse = map_dbl(conditional_coverage_glm,
      \(tibble) {
        sqrt(mean((predict(tibble, seq(0, 10, length.out = 1001)) - (1 - alpha_sig))^2))
      })
  ) |>
  prepare_for_plot(c("model", "method"))


#### Theorem 2 - Unconditional coverage 

results3 |>
  plot_unconditional(c("coverage", "leng")) +
  labs(y = "")
save_plot("unconditional.pdf", sub_dir = "simulations")


#### Theorem 3 – Conditional coverage

results3 |>
  filter(model %in% model_values) |>
  plot_unconditional(c("fsc", "ccmse")) +
  labs(y = "") +
  scale_y_continuous(breaks = scales::breaks_width(0.1)) +
  theme(strip.text.y = element_text(family = familyCaps))
save_plot("fsc_ccmse.pdf", sub_dir = "simulations")
results3 |>
  plot_unconditional(c("fsc", "ccmse")) +
  labs(y = "") +
  scale_y_continuous(breaks = scales::breaks_width(0.1)) +
  theme(strip.text.y = element_text(family = familyCaps))
save_plot("fsc_ccmse_full.pdf", sub_dir = "simulations")

results3 |>
  filter(n %in% n_values & model %in% model_values) |>
  plot_conditional("coverage") +
  labs(y = "Conditional coverage")
save_plot("conditional_coverage.pdf", sub_dir = "simulations")
results3 |>
  plot_conditional("coverage") +
  labs(y = "Conditional coverage")
save_plot("conditional_coverage_full.pdf", aspect = 4 / 3, sub_dir = "simulations")

results3 |>
  filter(n%in% n_values & model %in% model_values) |>
  plot_conditional("leng", scales = "free_y") +
  labs(y = "Conditional length")
save_plot("conditional_length.pdf", sub_dir = "simulations")
results3 |>
  plot_conditional("leng", scales = "free_y") +
  labs(y = "Conditional length")
save_plot("conditional_length_full.pdf", aspect = 4 / 3, sub_dir = "simulations")


#### Theorem 4 – Conditional coverage + Length improvement.

## Short table for chapter 4
results4 |>
  mutate(
    fsc = map_dbl(conditional, \(dt) min(dt$coverage)),
    ccmse = map_dbl(conditional_coverage_glm,
      \(tibble) {
        sqrt(mean((predict(tibble, seq(0, 10, length.out = 1001)) - (1 - alpha_sig))^2))
      })
  ) |>
  filter((method %in% c("IDR", "IDR*") & model %in% c("S1(Beta)", "S1(Uniform)"))
    | (method %in% c("QR", "QR*") & model %in% c("S1(Bound above)", "S1(No bounds)"))) |>
  filter(n %in% n_values) |>
  select(n, model, method, coverage, fsc, ccmse) |>
  arrange(n, model, method) |>
  mutate(
    model = renameForCSV(model),
    method = renameForCSV(method),
    coverage = round(coverage, 4),
    fsc = round(fsc, 4),
    ccmse = round(ccmse, 4)) |>
  rename(
    Model = "model", 
    Method = "method", 
    Coverage = "coverage", 
    ## NOTE 2024-10-10 Hard-coded `\abb{fsc}' and `\abb{ccmse}' here.
    "\\textsc{fsc}" = fsc, 
    "\\textsc{ccmse}" = ccmse
  ) |>
writeTable("results4CoverageShort.csv")

## Plot Length difference
results4 |>
  filter(method %in% c("IDR", "IDR*", "QR", "QR*")) |>
  group_by(model, n, method = substring(method, first=0, last=1)) |>
  mutate(method = if_else(method == "I", "IDR", "QR")) |>
  summarise(conditional = {
    ## conditional is a list of two dfs.
    ## Because dt is ordered by method, the first df comes from IDR (resp. QR)
    ## and the second df comes from IDR* (resp. QR*)
    ## We bind these two together, duplicating each `bin' value.
    ## Then, per bin, we calculate -(leng(IDR*) - leng(IDR)) / leng(IDR) (resp. -(leng(QR*) - leng(QR)) / leng(QR))
    ## via the call to `diff'.
    bind_rows(conditional) |>
      group_by(cut) |>
      summarise(leng = -diff(leng) / leng[1]) |>
      list()
  } , .groups = "drop") |>
  prepare_for_plot(c("model", "method")) |>
  filter(n %in% n_values) |>
  plot_conditional("leng", scales = "free_y") +
  theme(strip.text.y = element_text(family = family)) +
  labs(y = "Relative improvement")
save_plot("length_improvement.pdf", aspect = 4 / 3, sub_dir = "simulations")
