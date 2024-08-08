source("lib.R")

run_experiment(
  model = c("D", "P", "NI", "S", "AR(S)", "AR(NI)"),
  method = c("CP_OLS", "CP_LOC", "DR", "IDR", "QR"),
  n = 2^c(7:14),
  runs = 1024,
  alpha_sig = 0.1,
  sub_dir = "theorem-3"
)
