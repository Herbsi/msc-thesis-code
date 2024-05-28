library(tibble)
source("dcp.R")

## Analysis --------------------------------------------------------------------

analyse <- function(data_frame, split, sig = 0.1) {
  tibble(
    name = c("dcp-qr",  "dcp-dr", "dcp-idr", "dcp-idrbag", "cp-ols", "cp-loc"),
    fn = list(dcp_qr, dcp_dr, dcp_idr, dcp_idrbag, dcp_cp_ols, dcp_cp_loc),
    results = map(seq_along(name), ~ tibble(coverage = NULL, leng = NULL))
  ) |>
    mutate(results = map(fn, \(f) f(Y ~ X, data_frame, split, sig)))
}

evaluate <- function(data_frame) {
  data_frame |>
    mutate(
      ## TODO <2024-05-28 Tue>: Add `sd' and `avg length of IV'
      unconditional_coverage = map_dbl(results, \(df) summarise(df, mean(coverage)) |> pull()),
    )
}

## Plotting --------------------------------------------------------------------
         
bin <- function(data, base, num_bins) {
  ## TODO <2024-05-22 Wed> Currently, the final bin is odd.
  ## Bins data into `num_bins'-many bins.
  ## Each bin corresponds to a quantile bin of `base'.
  ## This assumes that the rows of `data' and `base' correspond to the same objecs.
  data |>
    mutate(base_rank = rank(base, ties.method = "random")) |>
    mutate(bin = floor((num_bins - 1) * base_rank / nrow(data)), .keep = "unused") |>
    group_by(bin)
}


