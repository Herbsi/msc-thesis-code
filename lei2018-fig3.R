#!/usr/bin/env Rscript

options(tidyverse.quiet = TRUE)

library(tidyverse)

source("plot-lib.R")

set.seed(42)

n <- 1000
iTrain <- sample(1:1000, 500)
iTest <- -iTrain

data_tibble <- tibble(
  X = runif(n, 0, 2*pi),
  Y = sin(X) + pi * abs(X) / 20 * rnorm(n)
)

ss <- smooth.spline(
  data_tibble[iTrain, ] |> pull(X),
  data_tibble[iTrain, ] |> pull(Y)
)

data_tibble <- data_tibble |>
  mutate(
    Ypred = predict(ss, X)$y,
    scoresUsual = abs(Y - Ypred)
  )

ssmad <- smooth.spline(
  data_tibble[iTrain, ] |> pull(X),
  data_tibble[iTrain, ] |> pull(scoresUsual)
)

data_tibble <- data_tibble |>
  mutate(
    scoresLOC = scoresUsual / abs(predict(ssmad, X)$y),
  )

threshUsual <- quantile(data_tibble$scoresUsual[iTest], 0.9 * (1 + 1 / 500))
threshLOC <- quantile(data_tibble$scoresLOC[iTest], 0.9 * (1 + 1 / 500))

glmUsual <- glm(scoresUsual <= threshUsual ~ X, data_tibble[iTest, ], family = binomial(link = "logit"))
glmLOC <- glm(scoresLOC <= threshLOC ~ X, data_tibble[iTest, ], family = binomial(link = "logit"))

data_tibble <- data_tibble |>
  mutate(
    coverageUsual = predict(glmUsual, data.frame(X), type = "response"),
    coverageLOC = predict(glmLOC, data.frame(X), type = "response"),
    threshUsual = 2 * threshUsual,
    threshLOC = 2 * threshLOC * abs(predict(ssmad, X)$y)
  )

### Plotting -------------------------------------------------------------------

## TODO: Add average coverage and length

## Predictions and ribbon

data_tibble |>
  pivot_longer(
    cols = c("threshUsual", "threshLOC"),
    names_to = c(".value", "variant"),
    names_pattern = "(thresh)(.*)"
  ) |>
  mutate(
    variant = if_else(variant == "LOC", "Locally adaptive", variant),
    variant = factor(variant, levels = c("Usual", "Locally adaptive"))
  ) |>
  ggplot() +
  facet_grid(cols = vars(variant)) +
  geom_ribbon(
    aes(x = X,
      ymin = Ypred - thresh / 2,
      ymax = Ypred + thresh / 2,
      fill = variant,
    ),
    alpha = 0.3
  ) +
  geom_point(
    aes(x = X, y = Y),
    size = 0.5
  ) +
  geom_line(
    aes(x = X, y = Ypred, colour = variant)
  ) +
  labs(
    x = "𝑋", # NOTE 2024-09-03 U+01D44B
    y = "𝑌", # NOTE 2024-09-03 U+01D44C
    colour = "",
    fill = ""
  ) +
  scale_dcp(limits = c("Usual", "Locally adaptive"), breaks = c("Usual", "Locally adaptive")) +
  theme_dcp() +
  theme(
    legend.text = element_text(family = family),
    axis.title.y = element_text(family = familyMath),
    strip.text = element_blank(),
  )
save_plot("Lei2018Fig3-Upper.pdf")


## Coverage and length

data_tibble |>
  pivot_longer(
    cols = c("coverageUsual", "coverageLOC", "threshUsual", "threshLOC"),
    names_to = c("plot", "variant"),
    names_pattern = "(coverage|thresh)(Usual|LOC)",
    values_to = "value"
  ) |>
  mutate(
    plot = if_else(plot == "coverage", "Conditional coverage", "Conditional length"),
    variant = if_else(variant == "LOC", "Locally adaptive", variant),
    variant = factor(variant, levels = c("Usual", "Locally adaptive"))
  ) |>
  ggplot() +
  facet_grid(rows = vars(plot), scales = "free_y") +
  geom_line(aes(x = X, y = value, colour = variant), linewidth = 0.5) +
  labs(
    x = "𝑋", # NOTE 2024-09-03 U+01D44B
    y = "",
    colour = ""
  ) +
  scale_dcp(limits = c("Usual", "Locally adaptive"), breaks = c("Usual", "Locally adaptive")) +
  theme_dcp() +
  theme(legend.text = element_text(family = family))
save_plot("Lei2018Fig3-Lower.pdf")
