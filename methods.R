################################################################################
## Reimplementations of distributional regression methods
################################################################################

library(quantreg)
library(purrr)

### Auxiliary functions

train.valid.split <- function(data) {
  ## Split data evenly `(X, Y)' into training set `(X0, Y0)' and calibration set `(X1, Y1)'
  ## TODO <2024-05-20 Mo> if `length(Y)' is odd, `(X0, Y0)' is one shorter than `(X1, Y1)'
  n <- nrow(data)

  return(
    list(data.train = data[1:floor(n/2), ],
      data.valid = data[(floor(n/2)+1):n, ] 
    )
  )
}


dcp.score <- function(ranks) { return(abs(ranks - 0.5)) }

dcp.threshold <- function(scores, alpha) {
  return(sort(scores)[ceiling((1 - alpha) * (1 + length(scores)))])
}


### Distributional regression functions

#### Quantile regression
dcp.qr <- function(data.train,
                   data.valid,
                   data.test,
                   alpha,
                   tau = seq(0.001, 0.999, length = 200)) {
  ## Fit QR model
  ## Note that rq is able to fit multiple values of `tau' at once.
  model <- rq(Y ~ X, data = data.train, tau = tau)

  ## Scoring with this model 
  local.score <- function(data, pred = predict(model, newdata = data)) {
    ## We calculate the rank as fraction of quantiles below the predicted values
    ## Hence rowMeans(...)
    return(dcp.score(rowMeans(pred <= data$Y)))
  }

  ## Calculate threshold based on validation set and calculate coverage on test set
  threshold <- dcp.threshold(local.score(data.valid), alpha)
  
  pred.test <- predict(model, newdata = data.test)

  ## Estimate coverage as test values within threshold
  coverage <- local.score(data.test, pred.test) <= threshold

  ## Calculate length of interval(s)
  leng <- apply(
    pred.test[, dcp.score(tau) <= threshold], # TODO <2024-05-20 Mo> I don't understand why we use these columns yet.
    1,
    \(row) max(row) - min(row)
  )

  leng[which(leng == -Inf)] <- NA

  return(list(coverage = coverage, leng = leng))
}


#### GLM-based DR
dcp.dr <- function(data.train, data.valid, data.test, tau, alpha) {
  ys <- quantile(unique(c(data.train$Y, data.valid$Y)), tau)
  ## We perform DR manually, by fitting a separate GLM for various values of y
  beta <- sapply(ys,
    \(y) glm((Y <= y) ~ X,
      data = data.train,
      family = binomial(link = "logit"))
    $coefficients)

  local.predict <- function(data) { return(plogis(cbind(1, data$X) %*% beta)) }

  local.score <- function(data, pred = local.predict(data)) {
    score <- rep(NA, nrow(data))
    for (i in 1:nrow(data)) {
      ## We obtain the PIT as the linear interpolation of the predicted data at the true output
      score[i] <- approx(x = ys,
        y = pred[i, ],
        xout = data[i, "Y"],
        rule=2)$y |>
              dcp.score()
    }
    return(score)
  }

  ## Calculate threshold based on validation set and calculate coverage on test set
  threshold <- dcp.threshold(local.score(data.valid), alpha)
  
  pred.test <- local.predict(data.test)

  ## Estimate coverage as test values within threshold
  coverage <- local.score(data.test, pred = pred.test) <= threshold

  ## Calculate length of interval(s)
  ## NOTE <2024-05-20 Mo> Calculating the length of the interval remains a mystery.
  leng <- rep(NA, nrow(data.test))
  for (i in 1:nrow(data.test)) {
    tmp <- ys[dcp.score(pred.test[i, ]) <= threshold]
    leng[i] <- max(tmp) - min(tmp)
  }

  leng[which(leng == -Inf)] <- NA

  return(list(coverage = coverage, leng = leng))
}


#### DCP-IDR

dcp.idr <- function(Y0, X0, Y1, X1, Y.test, X.test, alpha.sig) {
  ## TODO: Rewrite with new interface.
  fm <- idr(Y0, as_tibble(X0)) # Fit IDR to (`X0', `Y0')

  ## Scores on calibration set
  pred <- predict(fm, as_tibble(X1))
  cs <- abs(pit(pred, Y1) - 1 / 2)

  ## Calculate threshold as (1 - α) * (1 + |Y1|) quantile of the scores
  k <- ceiling((1 - alpha.sig) * (1 + length(Y1)))
  threshold <- sort(cs)[k]

  ## Scores on test set
  pred.test <- predict(fm, as_tibble(X.test))
  cs.test <- abs(pit(pred.test, Y.test) - 1 / 2)

  cov.idr <- cs.test <= threshold

  ## Calculate length of interval
  lb <- ub <- rep(NA, length(Y.test))
  for (i in 1:length(Y.test)) {
    ## NOTE 2024-05-19 I don't fully understand the calculation of the interval length yet.
    ys <- quantile(unique(c(Y0, Y1)), seq(0.001, 0.999, length=nrow(pred.test[[i]])))
    
    pred.i <- as_tibble(pred.test[[i]])
    indices <- pred.i |>
      mutate(indices = abs(cdf - 1 / 2) <= threshold, .keep = "unused") |>
      pull(indices)
      
    ci <- ys[indices]
    ub[i] <- max(ci)
    lb[i] <- min(ci)
  }

  leng.idr <- ub - lb
  leng.idr[which(leng.idr == -Inf)] <- NA
  
  return(list(
    cov.idr = cov.idr,
    leng.idr = leng.idr
    ))
}
