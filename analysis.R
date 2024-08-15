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

### Theorem 2 - Unconditional coverage -----------------------------------------

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

### Theorem 3 – Conditional coverage -------------------------------------------

pdf(file = "../tex/images/vectors/conditionalCoverage.pdf",
  width = 4 * 2.25,
  height = 3 * 2.25,
  pointsize = pointsize
)
results3 |>
  filter(n %in% n_values) |>
  filter(model %in% c("AR(NI)", "NI", "AR(P)", "S")) |>
  plot_conditional_coverage()
 dev.off()


pdf(file = "../tex/images/vectors/conditionalCoverageMSE.pdf",
  width = 4 * 2.25,
  height = 3 * 2.25,
  pointsize = pointsize
)
results3 |>
  filter(model %in% c("AR(NI)", "NI", "AR(P)", "S")) |>
  plot_conditional_coverage_mse()
dev.off()


pdf(file = "../tex/images/vectors/conditionalLeng.pdf",
  width = 4 * 2.25,
  height = 3 * 2.25,
  pointsize = pointsize
)
results3 |>
  filter(n %in% n_values) |>
  filter(model %in% c("AR(NI)", "NI", "AR(P)", "S")) |>
  plot_conditional_leng()
dev.off()

### Theorem 4 – Conditional coverage + Length improvement. ---------------------

rename_for_csv <- Vectorize(function(string) {
  rename_list <- list(
    ## TODO 2024-08-12 Output to tikz instead.
    "CP_LOC" = "\\textsc{\\idx{cp-loc}}",
    "CP_OLS" = "\\textsc{\\idx{cp-ols}}",
    "DR" = "\\abb{glmdr}",
    "IDR" = "\\abb{idr}",
    "QR" = "\\abb{qr}",
    "IDR*" = "\\dcpstar{\\abb{idr}}",
    "QR*" = "\\dcpstar{\\abb{qr}}",
    ##
    ## "AR(NI)" = "\\idx{model-ARNI}",
    ## "AR(P)" = "\\idx{model-ARP}",
    ## "AR(S)" = "\\idx{model-ARS}",
    ## "NI" = "\\idx{model-NI}",
    ## "P" = "\\idx{model-P}",
    ## "S" = "\\idx{model-S}",
    ##
    "S1(Beta)" = "\\idx{model-S1Beta}",
    "S1(Bound above)" = "\\idx{model-S1BA}",
    "S1(No bounds)" = "\\idx{model-S1NB}",
    "S1(Uniform)" = "\\idx{model-S1Unif}"
    )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


prepare_table_for_csv <- function(dt) {
  dt |>
    select(n, model, method, coverage, cc_mse) |>
    arrange(n, model, method) |>
    mutate(
      model = rename_for_csv(model),
      method = rename_for_csv(method),
      coverage = round(coverage, 4),
      cc_mse = round(cc_mse, 4)) |>
    rename(Model = "model", Method = "method", Coverage = "coverage", "\\textsc{ccmse}" = cc_mse)
}


targetDir <- file.path("..", "tex", "data")
dir.create(targetDir, recursive = TRUE)
## Short version for chapter 5
add_cc_mse(results4) |>
  filter((method %in% c("IDR", "IDR*") & model %in% c("S1(Beta)", "S1(Uniform)"))
    | (method %in% c("QR", "QR*") & model %in% c("S1(Bound above)", "S1(No bounds)"))) |>
  filter(n %in% n_values) |>
  prepare_table_for_csv() |>
  fwrite(file = file.path(targetDir, "results4CoverageShort.csv"), scipen=1000)

## Full version for Appendix
add_cc_mse(results4) |>
  prepare_table_for_csv() |>
  fwrite(file = file.path(targetDir, "results4CoverageFull.csv"), scipen=1000)

pdf(file = "../tex/images/vectors/conditionalLengDiff.pdf",
  width = 4 * 2.25,
  height = 3 * 2.25,
  pointsize = pointsize
)
results4 |>
  filter(n %in% n_values) |>
  plot_conditional_leng_diff()
dev.off()
