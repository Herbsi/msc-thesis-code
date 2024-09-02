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

#### Conditional coverage and length

## TODO: Add average coverage and length

data_tibble |>
  pivot_longer(
    cols = c("coverageUsual", "coverageLOC", "threshUsual", "threshLOC"),
    names_to = c("plot", "variant"),
    names_pattern = "(coverage|thresh)(Usual|LOC)",
    values_to = "value"
  ) |>
  mutate(
    plot = if_else(plot == "coverage", "Coverage", "Length"),
    variant = if_else(variant == "LOC", "Locally adaptive", variant),
    variant = factor(variant, levels = c("Usual", "Locally adaptive"))
  ) |>
  ggplot() +
  facet_grid(rows = vars(plot), scales = "free_y") +
  geom_line(aes(x = X, y = value, colour = variant)) +
  labs(colour = "", y = "") +
  theme_dcp() +
  theme(
    legend.text = element_text(family = family),
    axis.title.x = element_text(family = familyMath),
    axis.text = element_text(family = familyMath)
  )
savePlot("Lei2018Fig3-Lower.pdf")


#### Predictions and ribbon

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
  facet_wrap(vars(variant)) +
  geom_ribbon(
    aes(x = X,
      ymin = Ypred - thresh / 2,
      ymax = Ypred + thresh / 2,
      fill = variant,
    ),
    alpha = 0.3
  ) +
  geom_point(
    aes(x = X, y = Y)
  ) +
  geom_line(
    aes(x = X, y = Ypred, colour = variant)
  ) +
  labs(colour = "", fill = "") +
  theme_dcp() +
  theme(
    legend.text = element_text(family = family),
    axis.title.x = element_text(family = familyMath),
    axis.text = element_text(family = familyMath)
  )
savePlot("Lei2018Fig3-Upper.pdf")
