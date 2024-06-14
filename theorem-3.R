source("lib.R")
source("plots.R")

results_tibble <- theorem_3(
  runs = 10,
  alpha_sig = 0.1,
  n = 2^(5:7),
  model_name = c("D", "P", "NI", "S", "AR"))

plot_unconditional(results_tibble)
plot_conditional_coverage(results_tibble)
plot_conditional_leng(results_tibble)
plot_conditional_mse(results_tibble)
plot_conditional_sd(results_tibble)
