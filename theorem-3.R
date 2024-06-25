source("lib.R")
source("plots.R")

results_tibble_thm3 <- run_experiment("theorem-3",
  model_name = c("D", "P", "NI", "S", "AR(1)", "AR(2)"),
  method_name = c("QR", "DR", "IDR", "CP_OLS", "CP_LOC"),
  runs = 1,
  n = 2^(6:8))

plot_unconditional(results_tibble)
plot_conditional_coverage(results_tibble)
plot_conditional_leng(results_tibble)
plot_conditional_mse(results_tibble)
plot_conditional_sd(results_tibble)
