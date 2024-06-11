library(tidyverse)

df <- crossing(
  tibble(n = 4^(3:7)),
  tibble(model_name = c("D", "P", "NI", "S")),
  tibble(method_name = c("QR", "DR", "IDR", "CP_OLS", "CP_LOC"))
)
files <- str_c("results/theorem-3-euler/", df$n, df$model_name, df$method_name, sep = "_") |>
  str_c(".RData")

df2 <- tibble()
walk(files, \(f) {
  load(force(f), verbose = TRUE)
  df2 <<- bind_rows(df2, tribble(~coverage, ~leng, ~conditional, simulation_result$coverage, simulation_result$leng, simulation_result$conditional))
}
)

results_tibble <- bind_cols(df, df2)
save(results_tibble, file = str_c("results/theorem-3-euler/results_tibble", format(Sys.time(), "%Y%m%d%H%M%S"), ".RData"))
