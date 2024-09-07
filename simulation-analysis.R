library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(scales, warn.conflicts = FALSE)
library(tibble)
library(tidyr)

source("plot-lib.R")
source("table-lib.R")

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


mutate_ccmse <- function(dt, targetColumn = "ccmse", alpha_sig = 0.1, ...) {
  dt |>
    mutate({{ targetColumn }} := map_dbl(conditional_coverage,
      \(tibble) {
        sqrt(mean((predict(tibble, seq(0, 10, length.out = 1001)) - (1 - alpha_sig))^2))
      }), ...)
}


#### Unconditional

plot_unconditional <- function(dt) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels)) |>
    pivot_longer(cols = c(coverage, leng), names_to = "metric", values_to = "value") |>
    mutate(metric = if_else(metric == "coverage", "Coverage", "Length"))

  ggplot(dt) +
    facet_grid(metric ~ model, scales = "free_y") +
    geom_unconditional(y = value) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "") +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.x = element_text(family = familyCaps)
    )
}

plot_unconditional_coverage <- function(dt) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels))

  ggplot(dt) +
    facet_grid(model ~ .) +
    geom_unconditional(y = coverage) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Coverage") +
    theme(strip.text.y = element_text(family = familyCaps))
  ## TODO 2024-08-08 scale_y_log10() does not work for some reason.
}


plot_unconditional_leng <- function(dt) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels))

  ggplot(dt, aes(y = leng)) +
    facet_grid(model ~ ., scales = "free_y") +
    geom_unconditional(y = leng) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Length") +
    scale_y_log10() +
    theme(strip.text.y = element_text(family = familyCaps))
}


#### Conditional 

plot_conditional_coverage <- function(dt) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels)) |>
    mutate(conditional_coverage =
             ## I am bad at naming.  What happens here is that, the column
             ## `conditional_coverage' of `dt' is mutated to another column
             ## with the same name, but where each entry is now a tibble with
             ## two columns: `X' and `conditional_coverage'.
             ## `X' is a fine grid in [0, 10]; `conditional_coverage' is the
             ## GLM inside `dt$conditional_coverage' evaluated along that grid.
             map(conditional_coverage, \(tibble) {
               tibble(X = seq(0, 10, length.out = 1000),
                 conditional_coverage = predict(tibble, X))
             })) |>
    ## To plot this evaluated GLM, we unnest the tibble.
    unnest(conditional_coverage)

  ggplot(dt) +
    geom_conditional(y = conditional_coverage) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Conditional coverage") +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.y = element_text(family = familyCaps)
    )
}


plot_conditional_leng <- function(dt) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels)) |>
    unnest(conditional_leng, names_sep = ".") |>
    rename(conditional_leng = conditional_leng.leng) |>
    mutate(X = map_dbl(conditional_leng.bin, \(bin) {
      unlist(strsplit(gsub("[^0-9e.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    }))

  ggplot(dt) +
    geom_conditional(y = conditional_leng, scales = "free_y") +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Conditional length") +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      strip.text.y = element_text(family = familyCaps)
    )
}


#### Conditional things, aggregated over X

plot_ccmse <- function(dt, columnName = ccmse) {
  dt <- dt |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels))

  ggplot(dt) +
    facet_grid(model ~ .) +
    geom_unconditional(y = {{ columnName }}) +
    scale_dcp(breaks = unique(dt$method)) +
    scale_y_log10() +
    theme(
      axis.title.y = element_text(family = familyCaps),
      strip.text.y = element_text(family = familyCaps)
    )
}


plot_conditional_leng_diff <- function(dt) {
  dt <- dt |>
    filter(method %in% c("IDR", "IDR*", "QR", "QR*")) |>
    group_by(model, n, method = substring(method, first=0, last=1)) |>
    mutate(method = if_else(method == "I", "IDR", "QR")) |>
    summarise(conditional_leng = {
      ## conditional_leng is a list of two dfs.
      ## Because dt is ordered by method, the first df comes from IDR (resp. QR)
      ## and the second df comes from IDR* (resp. QR*)
      ## We bind these two together, duplicating each `bin' value.
      ## Then, per bin, we calculate -(leng(IDR*) - leng(IDR)) / leng(IDR) (resp. -(leng(QR*) - leng(QR)) / leng(QR))
      ## via the call to `diff'.
      bind_rows(conditional_leng) |>
        group_by(bin) |>
        summarise(conditional_leng = -diff(leng) / leng[1]) |>
        list()
    } , .groups = "drop") |>
    unnest(conditional_leng) |>
    mutate(X = map_dbl(bin, \(bin) {
      unlist(strsplit(gsub("[^0-9e.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    })) |>
    mutate(model = renameForPlot(model), method = renameForPlot(method)) |>
    mutate(method = factor(method, levels = methodLevels))

  ggplot(dt) +
    geom_conditional(
      y = conditional_leng,
      scales = "free_y"
    ) +
    scale_dcp(breaks = unique(dt$method)) +
    labs(y = "Relative improvement") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))
}


### Analysis -------------------------------------------------------------------

## TODO Turn these into command-line arguments
results3 <- readRDS("results/theorem-3-euler/20240809081639/result211829.rds")
results4 <- readRDS("results/theorem-4-euler/20240809085734/result_stitched20240812124821.rds")

n_values <- c(128, 1024, 16384)
model_values <- c("AR(NI)", "NI", "AR(P)", "S")


#### Theorem 2 - Unconditional coverage 

plot_unconditional(results3)
savePlot("unconditional.pdf")


#### Theorem 3 – Conditional coverage

results3 |>
  filter(n %in% n_values & model %in% model_values) |>
  plot_conditional_coverage()
savePlot("conditionalCoverage.pdf")

results3 |>
  mutate_ccmse() |>
  filter(model %in% model_values) |>
  plot_ccmse()
savePlot("CCMSE.pdf")

results3 |>
  filter(n%in% n_values & model %in% model_values) |>
  plot_conditional_leng()
savePlot("conditionalLeng.pdf")


### Theorem 4 – Conditional coverage + Length improvement. ---------------------

## Short version for chapter 5
results4 |>
  mutate_ccmse() |>
  filter((method %in% c("IDR", "IDR*") & model %in% c("S1(Beta)", "S1(Uniform)"))
    | (method %in% c("QR", "QR*") & model %in% c("S1(Bound above)", "S1(No bounds)"))) |>
  filter(n %in% n_values) |>
  select(n, model, method, coverage, ccmse) |>
  arrange(n, model, method) |>
  mutate(
    model = renameForCSV(model),
    method = renameForCSV(method),
    coverage = round(coverage, 4),
    ccmse = round(ccmse, 4)) |>
  rename(Model = "model", Method = "method", Coverage = "coverage", "\\textsc{ccmse}" = ccmse) |>
  writeTable("results4CoverageShort.csv")

## Plot Length difference
results4 |>
  filter(n %in% n_values) |>
  plot_conditional_leng_diff()
savePlot("conditionalLengthDiff.pdf", aspect = 3 / 2)
