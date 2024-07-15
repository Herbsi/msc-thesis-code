library(ggplot2)

source("lib.R")

plot_unconditional <- function(results_tibble) {
  results_tibble |>
    pivot_longer(cols = c(coverage, leng), names_to = "metric", values_to = "value") |>
    ggplot(aes(x = n, y = value, color = method_name, group = method_name)) +
    geom_line() +
    facet_grid(metric ~ model_name, scales = "free_y") +
    labs(
      title = "Coverage and Leng vs n for each Method and Model",
      x = "n",
      color = "Method",
    ) +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}

## Unconditional ----------------------------------------------------------------

geom_unconditional <- function(title, y, scales = "fixed") {
  list(geom_line(aes(x = n, color = method_name, group = method_name)),
      facet_grid(model_name ~ ., scales = scales),
      labs(title = title,
        x = "n",
        y = y,
        colour = "Method"),
      scale_x_continuous(trans = "log2"),
      theme_minimal())
}


plot_unconditional_coverage <- function(results_tibble) {
  ggplot(results_tibble,
    aes(y = coverage)) +
    geom_unconditional(
      title = "Unconditional coverage vs n for each method and model",
      y = "Coverage")
}


plot_unconditional_leng <- function(results_tibble) {
  ggplot(results_tibble,
    aes(y = leng)) +
    geom_unconditional(
      title = "Unconditional length vs n for each method and model",
      y = "Length",
      scales = "free_y")
}


## Conditional ------------------------------------------------------------------

geom_conditional <- function(title, y, scales = "fixed") {
  list(geom_line(aes(x = X, color = method_name)),
      facet_grid(model_name ~ n, scales = scales),
      labs(title = title,
        x = "X",
        y = y,
        color = "Method",
        fill = "Method"),
      theme_minimal()
  )
}


plot_conditional_coverage <- function(results_tibble) {
  results_tibble |> unnest(conditional_coverage) |>
    ggplot(aes(y = conditional_coverage)) +
    geom_conditional(
      title = "Conditional coverage by X for each combination of n, model, and method",
      y = "Conditional Coverage")
}


plot_conditional_leng <- function(results_tibble) {
  results_tibble |> unnest(conditional_leng) |>
    mutate(X = map_dbl(bin, \(bin) {
      unlist(strsplit(gsub("[^0-9e.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    })) |>
    ggplot(aes(y = conditional_leng)) +
    geom_conditional(
      title = "Conditional length by X for each combination of n, model, and method",
      y = "Conditional Length",
      scales = "free_y")
}


## Summaries --------------------------------------------------------------------

plot_conditional_coverage_mse <- function(results_tibble) {
  results_tibble |>
    conditional_coverage_mse(cc_mse) |>
    ggplot(aes(y = cc_mse)) +
    geom_unconditional(title = "MSE(coverage - 0.9) vs n for each Method and Model",
      y = "MSE(coverage - 0.9)")
}


plot_conditional_coverage_mae <- function(results_tibble) {
  results_tibble |>
    conditional_coverage_mae(cc_mae) |>
    ggplot(aes(y = cc_mae)) +
    geom_unconditional(title = "MAE(coverage - 0.9) vs n for each Method and Model",
      y = "MAE(coverage - 0.9)")
}


plot_conditional_coverage_sd <- function(results_tibble) {
  results_tibble |>
    conditional_coverage_sd(cc_sd) |>
    ggplot(aes(y = cc_sd)) +
    geom_unconditional(
      title = "sd(conditional coverage) vs n for each Method and Model",
      y = "sd(conditional coverage)")
}


plot_conditional_leng_sd <- function(results_tibble) {
  results_tibble |>
    conditional_leng_sd(cc_sd) |>
    ggplot(aes(y = cc_sd)) +
    geom_unconditional(
      title = "sd(conditional length) vs n for each Method and Model",
      y = "sd(conditional length)",
      scales = "free_y")
}
