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


plot_conditional_coverage <- function(results_tibble) {
  plot_tibble <- unnest(results_tibble, conditional_coverage)
  ggplot(plot_tibble, aes(x = X, y = conditional_coverage, color = method_name)) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Coverage by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Coverage",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}


plot_conditional_leng <- function(results_tibble) {
  plot_tibble <- unnest(results_tibble, conditional_coverage) |>
    mutate(X = map_dbl(bin, \(bin) {
      unlist(strsplit(gsub("[^0-9.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    }))
  ggplot(plot_tibble, aes(x = X, y = conditional_leng, color = method_name)) +
    geom_line() +
    facet_grid(model_name ~ n, scales = "free_y") +
    labs(title = "Leng by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Length",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}


plot_conditional_sd <- function(results_tibble) {
    plot_tibble <- results_tibble |>
      mutate(conditional_coverage = map_dbl(conditional_coverage, \(df) sd(df$conditional_coverage)))
    ggplot(plot_tibble, aes(x = n, y = conditional_coverage, color = method_name, group = method_name)) +
    geom_line() +
    facet_grid(~ model_name, scales = "free_y") +
    labs(
      title = "MSE(coverage) vs n for each Method and Model",
      x = "n",
      y = "MSE(coverage)",
      color = "Method",
      ) +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}


plot_conditional_mse <- function(results_tibble) {
  plot_tibble <- results_tibble |>
    mutate(conditional_coverage = map_dbl(conditional_coverage, \(df) sqrt(mean((df$conditional_coverage - 0.9)^2))))
  ggplot(plot_tibble,aes(x = n, y = conditional_coverage, color = method_name, group = method_name)) +
    geom_line() +
    facet_grid(~ model_name, scales = "free_y") +
    labs(
      title = "MSE(coverage) vs n for each Method and Model",
      x = "n",
      y = "MSE(coverage)",
      color = "Method",
      ) +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}
