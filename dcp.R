library(broom)
library(isodistrreg)
library(quantreg)

predict_glm_from_tidy <- function(tidy_glm, newdata) {
  intercept <- tidy_glm |> filter(term == "(Intercept)") |> pull(estimate)
  slope <- tidy_glm |> filter(term == "X") |> pull(estimate)
  plogis(intercept + slope * newdata)
}


dcp <- function(type, formula, data, split, alpha = 0.1) {
  with(split(data), {
    data_train <- train
    data_valid <- valid
    data_test <- test
    
    ## Fit model
    tau <- seq(0.001, 0.999, length = 200)
    ys <- quantile(unique(c(data_train$Y, data_valid$Y)), tau)
    
    fit <- dcp_fit(type, formula, data_train, alpha_sig = alpha, tau = tau, ys = ys)

    ## Calibrate model
    scores_valid <- dcp_score(fit, data_valid)
    threshold <- quantile(scores_valid, probs = min((1 - alpha) * (1 + 1 / length(scores_valid)), 1))

    ## Estimate coverage
    coverage <- dcp_score(fit, data_test) <= threshold
    leng <- diff(range(data_test$Y[coverage], na.rm = TRUE))
    
    ## Estimate conditional coverage as output of a logistic regression
    conditional_coverage <- tidy(suppressWarnings(glm(coverage ~ X, family = binomial(link = "logit"), data = data_test)))

    ## Learn conditional length – basically data compression
    conditional_leng <- dcp_leng(fit, data_test, threshold)
    conditional_leng[which(conditional_leng == -Inf)] <- NA
    conditional_leng <- data.frame(X = data_test$X, conditional_leng = conditional_leng)
    
    list(coverage = mean(coverage),
      leng = leng,
      conditional_coverage = conditional_coverage,
      conditional_leng = conditional_leng)
  })
}

dcp_qr <- function(formula, data, split, alpha = 0.1) {
  dcp("QR", formula, data, split, alpha)
}

dcp_qr_opt <- function(formula, data, split, alpha = 0.1) {
  dcp("QR*", formula, data, split, alpha)
}

dcp_dr <- function(formula, data, split, alpha = 0.1) {
  dcp("DR", formula, data, split, alpha)
}

dcp_idr <- function(formula, data, split, alpha = 0.1) {
  dcp("IDR", formula, data, split, alpha)
}

dcp_idrbag <- function(formula, data, split, alpha = 0.1) {
  dcp("IDR-BAG", formula, data, split, alpha)
}

dcp_cp_ols <- function(formula, data, split, alpha = 0.1) {
  dcp("CP-OLS", formula, data, split, alpha)
}

dcp_cp_loc <- function(formula, data, split, alpha = 0.1) {
  dcp("CP-LOC", formula, data, split, alpha)
}

### Fit ------------------------------------------------------------------------

dcp_fit <- function(type, formula, data, ...) {
  args <- list(...)
  switch(type,
    "QR" = dcp_fit.rqs(formula, data, args$tau),
    "DR" = dcp_fit.dr(formula, data, args$ys),
    "QR*" = dcp_fit.rq_opt(formula, data, args$alpha_sig, args$tau),
    "IDR" = dcp_fit.idrfit(formula, data, args$ys),
    "IDR-BAG" = dcp_fit.idrbag(formula, data),
    "CP-OLS" = dcp_fit.lm(formula, data),
    "CP-LOC" = dcp_fit.cp_loc(formula, data)
  )
}


dcp_predict <- function(fit, data) {
  UseMethod("dcp_predict")
}

dcp_bhat <- function(fit, data) {
  UseMethod("dcp_bhat")
}

dcp_score <- function(fit, data) {
  UseMethod("dcp_score")
}
dcp_leng <- function(fit, data, threshold) {
  UseMethod("dcp_leng")
}

### QR -------------------------------------------------------------------------

dcp_fit.rqs <- function(formula, data, tau) {
  rq(formula, tau = tau, data = data)
}

dcp_predict.rqs <- function(fit, data) {
  predict(fit, newdata = data)
}

dcp_score.rqs <- function(fit, data) {
  ## We calculate the rank as fraction of quantiles below the predicted values
  ## Hence rowMeans(...)
  abs(rowMeans(dcp_predict(fit, data) <= data[[names(fit$model)[1]]]) - 0.5)
}

dcp_leng.rqs <- function(fit, data, threshold) {
  apply(dcp_predict(fit, data)[, (abs(fit$tau - 0.5) <= threshold)],
    1,
    \(row) max(row) - min(row)
  )
}


### QR* ------------------------------------------------------------------------

dcp_fit.rq_opt <- function(formula, data, alpha_sig, tau) {
  rq <- rq(formula, tau = tau, data = data)
  
  fit <- list(rq = rq, alpha_sig = alpha_sig)
  class(fit) <- "rq_opt"
  fit
}

dcp_predict.rq_opt <- function(fit, data) {
  predict(fit$rq, newdata = data)
}

dcp_bhat.rq_opt <- function(fit, pred) {
  b_grid <- fit$rq$tau[fit$rq$tau <= fit$alpha_sig]
  target_tau <- b_grid + 1 - fit$alpha_sig

  compute_b_hat <- function(row) {
    leng <- approx(x = fit$rq$tau, y = row, xout = target_tau, rule = 2)$y - row[1:length(b_grid)]
    b_grid[which.min(leng)]
  }

  apply(pred, 1, compute_b_hat)
}

dcp_score.rq_opt <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  
  b_hat <- dcp_bhat(fit, pred)
  abs(rowMeans(pred <= data$Y) - b_hat - (1 - fit$alpha_sig) / 2)
}

dcp_leng.rq_opt <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  b_hat <- dcp_bhat(fit, pred)

  compute_leng <- function(row) {
    row[-1][abs(fit$rq$tau - row[1] - (1 - fit$alpha_sig / 2)) <= threshold] |> range() |> diff()
  }

  apply(cbind(b_hat, pred), 1, compute_leng)
}


### GLM-DR ---------------------------------------------------------------------

dcp_fit.dr <- function(formula, data, ys) {
  y <- data$Y
  x <- cbind(1, data[, "X"])
  beta <- sapply(ys,
    \(the_y) suppressWarnings(
      glm.fit(x, (y <= the_y), family = binomial(link = "logit"))$coefficients))
  fit <- list(beta = beta, ys = ys)
  class(fit) <- "dr"
  fit
}

dcp_predict.dr <- function(fit, data) {
  plogis(as.matrix(cbind(1, data[, "X"])) %*% fit$beta)
}

dcp_score.dr <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  imap_dbl(data$Y,
    \(y, idx) approx(x = fit$ys, y = pred[idx, ], xout = y, rule = 2)$y
    - 0.5) |>
    abs()
}

dcp_leng.dr <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  leng <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    tmp <- fit$ys[abs(pred[i, ] - 0.5) <= threshold]
    leng[i] <- max(tmp) - min(tmp)
  }
  leng
}

### IDR ------------------------------------------------------------------------

dcp_fit.idrfit <- function(formula, data, ys) {
  y <- data[[formula[[2]]]]
  x <- data[, formula[[3]]]
  fit <- idr(y, x)
  fit$ys <- ys
  fit
}

dcp_predict.idrfit <- function(fit, data) {
  predict(fit, data)
}

dcp_score.idrfit <- function(fit, data) {
  abs(pit(dcp_predict(fit, data), data$Y) - 0.5)
}

dcp_leng.idrfit <- function(fit, data, threshold) {
  cdf(dcp_predict(fit, data), fit$ys) |>
    apply(1, \(row) diff(range(fit$ys[abs(row - 0.5) <= threshold])))
}

### IDR-BAG --------------------------------------------------------------------

dcp_fit.idrbag <- function(formula, data) {
  y <- data$Y
  x <- data[, "X"]
  fit <- function(data) {
    ## NOTE <2024-05-28 Tue>: Used arbitrary values for `b' and `p' here.
    ## NOTE <2024-05-28 Tue>: Chose small values; otherwise, it takes forever.
    ## Also didn't see much improvement
    idrbag(y, x, newdata = data, b = 5, p = 0.8)
  }
  class(fit) <- "idrbag"
  fit
}

dcp_predict.idrbag <- function(fit, data) {
  fit(data)
}

dcp_score.idrbag <- function(fit, data) {
  abs(pit(dcp_predict(fit, data), data$Y) - 0.5)
}

dcp_leng.idrbag <- function(fit, data, threshold) {
  dcp_predict(fit, data) |>
    map_dbl(~ {
      tmp <- .x$points[abs(.x$cdf - 0.5) <= threshold]
      max(tmp) - min(tmp)
    }
    )
}


### CP-OLS ---------------------------------------------------------------------

dcp_fit.lm <- function(formula, data) {
  lm(formula, data)
}

dcp_predict.lm <- function(fit, data) {
  predict(fit, data)
}

dcp_score.lm <- function(fit, data) {
  y_name <- names(fit$model)[1]
  abs(data[[y_name]] - predict(fit, data))
}

dcp_leng.lm <- function(fit, data, threshold) {
  rep(2 * threshold, nrow(data))
}

### CP-loc ---------------------------------------------------------------------

dcp_fit.cp_loc <- function(formula, data) {
  model_reg <- lm(formula, data = data)
  model_sig <- lm(abs(residuals(model_reg)) ~ X, data = data)

  model <- list(reg = model_reg, sig = model_sig)
  class(model) <- "cp_loc" # I have succumb to the dark side---they had cookies.
  model # Is a list of two `lm's
}

dcp_predict.cp_loc <- function(fit, data) {
  list(reg = predict(fit$reg, data), sig = predict(fit$sig, data))
}

dcp_score.cp_loc <- function(fit, data) {
  y_name <- names(fit$reg$model)[1]
  pred <- dcp_predict(fit, data)
  abs(pred$reg - data[[y_name]]) / abs(pred$sig)
}

dcp_leng.cp_loc <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  2 * abs(pred$sig) * threshold
}

