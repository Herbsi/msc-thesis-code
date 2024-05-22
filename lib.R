train_valid_split <- function(data) {
  ## Split data evenly `(X, Y)' into training set `(X0, Y0)' and calibration set `(X1, Y1)'
  ## TODO <2024-05-20 Mo> if `length(Y)' is odd, `(X0, Y0)' is one shorter than `(X1, Y1)'
  n <- nrow(data)

  return(
    list(data.train = data[1:floor(n/2), ],
      data.valid = data[(floor(n/2)+1):n, ] 
    )
  )
}


dcp_score <- function(ranks) {
  return(abs(ranks - 0.5))
}


dcp_threshold <- function(scores, alpha) {
  return(sort(scores)[ceiling((1 - alpha) * (1 + length(scores)))])
}


bin <- function(data, base, num_bins) {
  ## Bins data into `num_bins'-many bins.
  ## Each bin corresponds to a quantile bin of `base'.
  ## This assumes that the rows of `data' and `base' correspond to the same objecs.
  return(data |>
           mutate(base_rank = rank(base, ties.method = "random")) |>
           mutate(bin = floor((num_bins - 1) * base_rank / nrow(data)), .keep = "unused") |>
           group_by(bin))
}


