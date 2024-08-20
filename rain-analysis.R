library(data.table)
library(tidyverse)

source("plots.R") # rename_for_plot

### Load results

resultdcp <- readRDS("results/precipitation/20240819135432/resultdcp.rds")
resultziegel <- readRDS("results/precipitation/20240819135432/resultziegel.rds")


### Create joined result

resultdcp[, approach := "dcp"]
resultziegel[, approach := "ziegel"]
results <- rbindlist(list(resultdcp, resultziegel))

### Setup target directories

tableDir <- file.path("..", "tex", "data")
dir.create(tableDir, recursive = TRUE)
plotDir <- file.path("..", "tex", "images")
dir.create(plotDir, recursive = TRUE)

### Setup plot configurations

pointsize <- 11
textwidth <- 418.2555 # NOTE <2024-05-22 Wed> Hard-coded `\the\textwidth' from LaTeX document here.

### Helper functions

rename_for_csv <- Vectorize(function(string) {
  rename_list <- list(
    "CP_LOC" = "\\textsc{\\idx{cp-loc}}",
    "CP_OLS" = "\\textsc{\\idx{cp-ols}}",
    "DR" = "\\textsc{glmdr}", # HACK 2024-08-20 This should be \\abb{glmdr}, but this caused issues in the tables in LaTeX.
    ## But \\abb{idr} works for some reason
    "IDR" = "\\abb{idr}",
    "QR" = "\\abb{qr}",
    "IDR*" = "\\dcpstar{\\abb{idr}}",
    "QR*" = "\\dcpstar{\\abb{qr}}"
    )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


### Unconditional table

results |>
  select(airport, horizon, method, approach, coverage, leng) |>
  mutate(
    coverage = round(100 * coverage, 1),
    leng = round(leng, 3)
  ) |>
  dcast(method + horizon ~ airport + approach, value.var = c("coverage", "leng")) |>
  arrange(method, horizon) |>
  mutate(method = rename_for_csv(method)) |>
  fwrite(file = file.path(tableDir, "rainUncond.csv"), scipen=1000)


### Unconditional graph

pdf(file = file.path(plotDir, "rainUncond.pdf"),
  width = textwidth / 72.27,
  pointsize = pointsize
)
results |>
  rename(Coverage = coverage, Length = leng) |>
  pivot_longer(cols = c(Coverage, Length),
    names_to = "metric", values_to = "value") |>
  mutate(
    airport = rename_for_plot(airport),
    method = rename_for_plot(method),
    approach = rename_for_plot(approach)
  ) |>
  ggplot() +
  geom_point(aes(x = horizon, y = value, color = method, group = method)) +
  facet_grid(metric ~ airport + approach, scales = "free_y") +
  labs(
    x = "Horizon",
    y = "",
    shape = "",
    color = "Method",) +
  theme_minimal()
dev.off()


### CCMSE Table

results |>
  select(
    airport,
    horizon,
    method,
    approach,
    conditional_coverage_mse_full,
    conditional_coverage_mse_hres
  ) |>
  mutate(
    conditional_coverage_mse_full = round(conditional_coverage_mse_full, 3),
    conditional_coverage_mse_hres = round(conditional_coverage_mse_hres, 3)
  ) |>
  dcast(
    method + horizon ~ airport + approach,
    value.var = c("conditional_coverage_mse_full", "conditional_coverage_mse_hres")
  ) |>
  arrange(method, horizon) |>
  mutate(method = rename_for_csv(method)) |>
  fwrite(file = file.path(tableDir, "rainCCMSE.csv"), scipen = 1000)


### CCMSE Graph

pdf(file = file.path(plotDir, "rainCCMSE.pdf"),
  width = textwidth / 72.27, # Transform from pt to in.
  pointsize = pointsize
)
resultziegel |>
  pivot_longer(
    cols = c(conditional_coverage_mse_full, conditional_coverage_mse_hres),
    names_to = "mse_type", values_to = "value"
  ) |>
  mutate(
    airport = rename_for_plot(airport),
    mse_type = rename_for_plot(mse_type),
    method = rename_for_plot(method)
  ) |>
  ggplot() +
  geom_point(aes(
    x = horizon,
    y = value,
    color = method,
    group = method,
    shape = mse_type
  )) +
  facet_grid(airport ~ ., scales = "fixed") +
  labs(
    x = "Horizon",
    y = "",
    shape = "",
    color = "Method",) +
  theme_minimal()
dev.off()


### Coverage and Length vs Date plot
## For Full and HRES

pdf(file = file.path(plotDir, "rainDateZRH2.pdf"),
  width = textwidth / 72.27,
  pointsize = pointsize
)
## FIXME 2024-08-20 HRES length is not being plotted.
resultziegel |>
  filter(horizon == 2 & airport == "zrh") |>
  select(method, conditional_coverage, conditional_leng) |>
  unnest(c(conditional_coverage, conditional_leng), names_sep = ".") |>
  select(!conditional_leng.date) |>
  rename(date = conditional_coverage.date) |>
  group_by(
    method, year = year(date), month = month(date)) |>
  summarise(
    full_cov = mean(conditional_coverage.conditional_coverage_full),
    hres_cov = mean(conditional_coverage.conditional_coverage_hres),
    full_leng = mean(conditional_leng.conditional_leng, na.rm = TRUE),
    hres_leng = mean(conditional_leng.hres, na.rm = TRUE),
    .groups = "drop",
  ) |>
  pivot_longer(
    cols = c(full_cov, hres_cov, full_leng, hres_leng),
    names_to = "type", values_to = "value") |>
  mutate(
    metric = as.factor(if_else(str_sub(type, 6, -1) == "cov", "Coverage", "Length")),
    type = as.factor(if_else(str_sub(type, 1, 4) == "full", "Full", "HRES")),
    date = as_date(str_c(year, month, "01", sep = "-")),
    coverage_type = as.factor(type),
    method = rename_for_plot(method)) |>
  ggplot() +
  geom_line(
    aes(x = date, y = value, color = method, linetype = type)
  ) +
  facet_grid(rows = c("metric"), scales = "free_y") +
  labs(
    x = "Date",
    y = "",
    color = "Method",
    linetype = "",
  ) +
  theme_minimal()
dev.off()
