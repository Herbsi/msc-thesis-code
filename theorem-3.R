source("lib.R")
source("plots.R")

results_tibble <- theorem_3(runs = 10, n = 2^(5:10), alpha_sig = 0.1)
pred_tibble <- pred_conditional(results_tibble)

plot_unconditional(results_tibble)


