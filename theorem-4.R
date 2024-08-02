source("lib.R")

run_experiment(
  model = c("S1(Uniform)", "S1(Beta)", "S1(Bound above)", "S1(No bounds)"),
  method = c("CP_OLS", "CP_LOC", "IDR", "IDR*", "QR", "IDR*"),
  n = 2^c(7:14),
  runs = 512,
  alpha_sig = 0.1,
  sub_dir = "theorem-4")
