library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(tibble)
library(tidyr)

rename_for_tex <- Vectorize(function(string) {
  rename_list <- list(
    ## TODO 2024-08-12 Output to tikz instead.
    "CP_LOC" = "CP-LOC",
    "CP_OLS" = "CP-OLS",
    "DR" = "GLMDR"
    ## "CP_LOC" = "\\textsc{\\idx{cp-loc}}",
    ## "CP_OLS" = "\\textsc{\\idx{cp-ols}}",
    ## "DR" = "\\abb{glmdr}",
    ## "IDR" = "\\abb{idr}",
    ## "QR" = "\\abb{qr}",
    ## "IDR*" = "\\abb{idr}\\(\\star\\)",
    ## "QR*" = "\\abb{qr}\\(\\star\\)",
    ##
    ## "AR(NI)" = "\\idx{model-ARNI}",
    ## "AR(P)" = "\\idx{model-ARP}",
    ## "AR(S)" = "\\idx{model-ARS}",
    ## "NI" = "\\idx{model-NI}",
    ## "P" = "\\idx{model-P}",
    ## "S" = "\\idx{model-S}",
    ##
    ## "S1(Beta)" = "\\idx{model-S1Beta}",
    ## "S1(Bound above)" = "\\idx{model-S1BA}",
    ## "S1(No bounds)" = "\\idx{model-S1NB}",
    ## "S1(Uniform)" = "\\idx{model-S1Unif}"
    )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


plot_unconditional <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    pivot_longer(cols = c(coverage, leng), names_to = "metric", values_to = "value") |>
    ggplot(aes(x = n, y = value, color = method, group = method)) +
    geom_line() +
    facet_grid(metric ~ model, scales = "free_y") +
    labs(
      x = "n",
      color = "Method",
    ) +
    scale_x_continuous(trans = "log2") +
    theme_minimal()
}

### Unconditional --------------------------------------------------------------

geom_unconditional <- function(title, y, scales = "fixed") {
  list(geom_line(aes(x = n, color = method, group = method)),
      facet_grid(model ~ ., scales = scales),
      labs(title = title,
        x = "n",
        y = y,
        colour = "Method"),
      scale_x_continuous(transform = "log2"),
      theme_minimal())
}


plot_unconditional_coverage <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    ggplot(aes(y = coverage)) +
    geom_unconditional(
      title = "Unconditional coverage vs n for each method and model",
      y = "Coverage")
  ## TODO 2024-08-08 scale_y_log10() does not work for some reason.
}


plot_unconditional_leng <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    ggplot(aes(y = leng)) +
    geom_unconditional(
      title = "Unconditional length vs n for each method and model",
      y = "Length",
      scales = "free_y") +
    scale_y_log10()
}


### Conditional ----------------------------------------------------------------

predict.tbl_df <- function(object, newdata) {
  intercept <- object |> filter(term == "(Intercept)") |> pull(estimate)
  slope <- object |> filter(term == "X") |> pull(estimate)
  linear_predictor <- intercept + slope * newdata
  plogis(linear_predictor)
}


geom_conditional <- function(title, y, scales = "fixed") {
  list(geom_line(aes(x = X, color = method)),
      facet_grid(model ~ n, scales = scales),
      labs(title = title,
        x = "X",
        y = y,
        color = "Method",
        fill = "Method"),
      theme_minimal()
  )
}


plot_conditional_coverage <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
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
    unnest(conditional_coverage) |>
    ggplot(aes(y = conditional_coverage)) +
    geom_conditional(
      title = "Conditional coverage by X for each combination of n, model, and method",
      y = "Conditional Coverage")
}


plot_conditional_leng <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    unnest(conditional_leng, names_sep = ".") |>
    rename(conditional_leng = conditional_leng.leng) |>
    mutate(X = map_dbl(conditional_leng.bin, \(bin) {
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


### Conditional things, aggregated over X --------------------------------------

plot_conditional_coverage_mse <- function(dt) {
  dt |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    mutate(cc_mse = map_dbl(conditional_coverage,
      \(tibble) {
        sqrt(mean((predict(tibble, seq(0, 10, length.out = 1000)) - 0.9)^2))
      })) |>
    ggplot(aes(y = cc_mse)) +
    geom_unconditional(
      title = "MSE(coverage - 0.9) vs n for each Method and Model",
      y = "MSE(coverage - 0.9)") +
    scale_y_log10()
}


## TODO 2024-08-08 Think of a way to compare interval lengths, provided the coverage is good.


plot_conditional_leng_diff <- function(dt) {
  ## TODO Plot relative improvement instead of absolute difference
  asinh_trans <- trans_new("asinh",
    transform = function(x) asinh(x),
    inverse = function(x) sinh(x))

  dt |>
    filter(method %in% c("IDR", "IDR*", "QR", "QR*")) |>
    group_by(model, n, method = substring(method, first=0, last=1)) |>
    mutate(method = if_else(method == "I", "IDR", "QR")) |>
    summarise(conditional_leng = {
      ## conditional_leng is a list of two dfs.
      ## Because dt is ordered by method, the first df comes from IDR (resp. QR)
      ## and the second df comes from IDR* (resp. QR*)
      ## We bind these two together, duplicating each `bin' value.
      ## Then, per bin, we calculate -(leng(IDR*) - leng(IDR)) (resp. -(leng(IDR*) - leng(IDR)))
      ## via the call to `diff'.
      bind_rows(conditional_leng) |>
        group_by(bin) |>
        summarise(conditional_leng = -diff(leng)) |>
        list()
    } , .groups = "drop") |>
    unnest(conditional_leng) |>
    mutate(X = map_dbl(bin, \(bin) {
      unlist(strsplit(gsub("[^0-9e.,-]", "", bin), ",")) |>
        as.numeric() |>
        mean()
    })) |>
    mutate(model = rename_for_tex(model), method = rename_for_tex(method)) |>
    ggplot(aes(y = conditional_leng)) +
    geom_conditional(
      title = "Conditional Length Improvement",
      y = "Conditional Length Difference (Regular - Optimal)",
      scales = "free_y") +
    scale_y_continuous(transform = asinh_trans)
}
