# DCP
Code for my master’s thesis ‘Distributional conformal prediction’ (DCP).

The thesis is an (re-)implementation of

Chernozhukov, Victor, Kaspar Wüthrich, and Yinchu Zhu. "Distributional conformal prediction." Proceedings of the National Academy of Sciences 118.48 (2021): e2107794118.

with additional methods for estimating conditional distribution functions, in particular, IDR (Henzi, Alexander, Johanna F. Ziegel, and Tilmann Gneiting. "Isotonic distributional regression." Journal of the Royal Statistical Society Series B: Statistical Methodology 83.5 (2021): 963-993.)

`dcp.R` contains the implementation DCP.

`lib.R` contains `run_experiment` which applies some DCP methods to some data models multiple times at some sample sizes (these are all parameters).
See `theorem-3.R` and `theorem-4.R` on how to use it.

`rain.R` applies the DCP methods to statistical post processing of precipitation data.

