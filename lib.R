library(tibble)
source("dcp.R")

## Analysis --------------------------------------------------------------------

analyse <- function(data_frame, alpha = 0.1) {
  tibble(
    name = c("dcp-qr",  "dcp-dr", "dcp-idr", "dcp-idrbag", "cp-ols", "cp-loc"),
    fn = list(dcp_qr, dcp_dr, dcp_idr, dcp_idrbag, dcp_cp_ols, dcp_cp_loc),
    results = map(seq_along(name), ~ tibble(coverage = NULL, leng = NULL))
  ) |>
    mutate(results = map(fn, ~ .x(Y ~ X, data_frame, split, alpha)))
}


## Plotting --------------------------------------------------------------------
         
bin <- function(data, base, num_bins) {
  ## Bins data into `num_bins'-many bins.
  ## Each bin corresponds to a quantile bin of `base'.
  ## This assumes that the rows of `data' and `base' correspond to the same objecs.
  return(data |>
           mutate(base_rank = rank(base, ties.method = "random")) |>
           mutate(bin = floor((num_bins - 1) * base_rank / nrow(data)), .keep = "unused") |>
           group_by(bin))
}


