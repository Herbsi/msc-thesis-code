#!/usr/bin/env Rscript

library(ggplot2)

source("plot-lib.R")

alpha_sig <- 0.1

qR <- quantile(abs(runif(100000) * rnorm(100000)), probs = 1 - alpha_sig)

X <- seq(0, 1, length.out = 1000)

ggplot(data = tibble::tibble(X, cc = 2 * pnorm(qR / X) - 1)) +
  geom_line(aes(x = X, y = cc), linewidth = 0.5) +
  labs(
    x = "𝑋",
    y = "Conditional coverage"
  ) +
  theme_dcp()
save_plot("dcp-fig1.pdf", aspect = 9 / 16)
