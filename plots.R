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


plot_conditional_coverage <- function(pred_tibble) {
  plot_tibble <- unnest(calc_conditional_per_X(pred_tibble), conditional, names_sep = "_")
  ggplot(plot_tibble, aes(x = conditional_X, y = conditional_coverage, color = method_name)) +
    ## geom_ribbon(aes(ymin = conditional_coverage - cc_std, ymax = conditional_coverage + cc_std, fill = method_name), alpha = 0.2) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Coverage by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Coverage",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}


plot_conditional_leng <- function(pred_tibble) {
  plot_tibble <- unnest(calc_conditional_per_X(pred_tibble), conditional, names_sep = "_")
  ggplot(plot_tibble, aes(x = conditional_X, y = conditional_leng, color = method_name)) +
    ## geom_ribbon(aes(ymin = conditional_coverage - cc_std, ymax = conditional_coverage + cc_std, fill = method_name), alpha = 0.2) +
    geom_line() +
    facet_grid(n ~ model_name) +
    labs(title = "Leng by X for each combination of n, model, and method",
      x = "X",
      y = "Conditional Length",
      color = "Method",
      fill = "Method") +
    theme_minimal()
}

