library(data.table)
## TODO 2024-08-12 `tikzDevice' does not work this way (yet).
library(tikzDevice)
options(tikzDefaultEngine = "luatex")
options(tikzDocumentDeclaration = "\\documentclass[11pt,a4paper,twoside,openright]{turtwig}")
options(tikzLualatexPackages = c(
  "\\usepackage[active,tightpage,psfixbb]{preview}\n",
  "\\PreviewEnvironment{pgfpicture}\n",
  "\\setlength\\PreviewBorder{0pt}\n"
))
options(tikzUnicodeMetricPackages = c(
  "\\usetikzlibrary{calc}\n"
))

source("plots.R")

results3 <- readRDS("results/theorem-3-euler/20240809081639/result211829.rds")
results4 <- readRDS("results/theorem-4-euler/20240809085734/result_stitched20240812124821.rds")

n_values <- c(128, 1024, 16384)

pointsize <- 11
textwidth <- 418.2555 # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.

### Theorem 2 - Unconditional coverage

pdf(file = "../tex/images/vectors/unconditionalCoverage.pdf",
  width = textwidth / 72.27, # Transform from pt to in.
  paper = "a4",
  pointsize = pointsize
)
plot_unconditional_coverage(results3)
dev.off()

pdf(file = "../tex/images/vectors/uncoditionalLeng.pdf",
  width = textwidth / 72.27, # Transform from pt to in.
  paper = "a4",
  pointsize = pointsize
)
plot_unconditional_leng(results3)
dev.off()

## What I did this:
## I picked the (arbitrary) aspect ration of 4 : 3 (width : height)
## and the (arbitrary, but same as in LaTeX) point size of 11.
## Then I tried multiple factors (I settled on 2.25 for now)
## until the graphic looked good.
## This means that the point size (and font family)
## are *completely different* from the LaTeX document
## TODO 2024-08-12 Use a different output device so that font matches LaTeX.
pdf(file = "../tex/images/vectors/unconditional.pdf",
  width = 4 * 2.25,
  height = 3 * 2.25,
  pointsize = pointsize
)
plot_unconditional(results3)
dev.off()

### Theorem 3 – Conditional coverage

results3 |>
  ## TODO 2024-08-08 Subset models here.
  filter(n %in% n_values) |>
  plot_conditional_coverage()
dev.off()

results3 |>
  filter(n %in% n_values) |>
  plot_conditional_leng()

results3 |>
  plot_conditional_coverage_mse()

### Theorem 4 – Conditional coverage + Length improvement.

results4 |>
  ## filter(n %in% n_values) |>
  plot_conditional_coverage_mse()

results4 |>
  filter(n %in% n_values) |>
  plot_conditional_leng()

results4 |>
  filter(n %in% n_values) |>
  plot_conditional_leng_diff()
