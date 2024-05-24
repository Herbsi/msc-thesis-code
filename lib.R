bin <- function(data, base, num_bins) {
  ## Bins data into `num_bins'-many bins.
  ## Each bin corresponds to a quantile bin of `base'.
  ## This assumes that the rows of `data' and `base' correspond to the same objecs.
  return(data |>
           mutate(base_rank = rank(base, ties.method = "random")) |>
           mutate(bin = floor((num_bins - 1) * base_rank / nrow(data)), .keep = "unused") |>
           group_by(bin))
}


