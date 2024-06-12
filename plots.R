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


plot_sd <- function(pred_tibble) {
  calc_uncond_coverage_sd(pred_tibble) |>
    ggplot(aes(x = n, y = coverage_sd, color = method_name, group = method_name)) +
    geom_line() +
    facet_wrap(~ model_name) +
    labs(title = "sd(coverage) vs n for each Method and Model",
      x = "n",
      y = "sd(coverage)",
      color = "Method") +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}



plot_cond_coverage <- function(pred_tibble) {
  plot_tibble <- unnest(calc_coverage_per_X(pred_tibble), conditional)
  ggplot(plot_tibble, aes(x = X, y = conditional_coverage, color = method_name)) +
    ## geom_ribbon(aes(ymin = conditional_coverage - cc_std, ymax = conditional_coverage + cc_std, fill = method_name), alpha = 0.2) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Predictions by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Coverage",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}


plot_cond_sd <- function(pred_tibble) {
  calc_coverage_per_X(pred_tibble) |>
    unnest(conditional) |>
    ggplot(aes(x = X, y = cc_std, color = method_name)) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "SD by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional SD",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}
