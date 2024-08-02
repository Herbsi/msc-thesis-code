library(data.table, warn.conflicts=FALSE)
library(purrr, include.only = c("imap_dbl"))
library(isodistrreg)
library(quantreg)

dcp <- function(method, formula, data_train, data_valid, data_test, alpha_sig = 0.1, ...) {
  ## Remaining arguments `...' get passed on to `dcp_fit'.

  ## Extract column name of response
  Y <- deparse(formula[[2]])

  tau <- seq(0.001, 0.999, length = 2000)
  ys <- quantile(unique(data_test[, Y, env = list(Y = Y)]), tau) # This extracts the `Y' column as a vector.

  ## Fit model
  fit <- dcp_fit(method, formula, data_train, alpha_sig = alpha_sig, tau = tau, ys = ys, ...)

  ## Calibrate model
  scores_valid <- dcp_score(fit, data_valid)
  threshold <- quantile(scores_valid, probs = min((1 - alpha_sig) * (1 + 1/length(scores_valid)), 1))
  
  ## Evaluate model
  data_test[,
    `:=`(conditional_coverage = dcp_score(fit, .SD) <= threshold,
      conditional_leng = {
        leng <- dcp_leng(fit, .SD, threshold)
        leng[leng == -Inf] <- NA
        leng
      })
  ][]
}


dcp_fit <- function(method, formula, data, ...) {
  args <- list(...)
  switch(method,
    "CP_LOC" = dcp_fit.cp_loc(formula, data),
    "CP_OLS" = dcp_fit.lm(formula, data),
    "DR" = dcp_fit.dr(formula, data, args$ys),
    "IDR" = dcp_fit.idrfit(formula, data, ys = args$ys, groups = args$groups, orders = args$orders),
    "IDR*" = dcp_fit.idrfit_opt(formula, data, args$alpha_sig, ys = args$ys, tau = args$tau, groups = args$groups, orders = args$orders),
    "QR" = dcp_fit.rqs(formula, data, args$tau),
    "QR*" = dcp_fit.rq_opt(formula, data, args$alpha_sig, args$tau)
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
  pred <- predict(fit, newdata = data)
  ## Make sure `pred' is a `nrow(data)' × `length(fit$tau)' matrix,
  ## even if `nrow(data) == 1' so that pred becomes an atomic vector.
  if (is.null(dim(pred))) {
    t(as.matrix(pred))
  } else {
    pred
  }
}

dcp_score.rqs <- function(fit, data) {
  ## We calculate the rank as fraction of quantiles below the predicted values
  ## Hence rowMeans(...)
  abs(rowMeans(dcp_predict(fit, data) <= data[[names(fit$model)[1]]]) - 0.5)
}

dcp_leng.rqs <- function(fit, data, threshold) {
  ## This uses that τ = F(Q(τ | X) | X), ie uses τ directly as the dcp_score of Q(τ | X)
  ## and then takes as the length of the predicted interval the maximal accepted quantile minus the minimal accepted quantile
  apply(dcp_predict(fit, data)[, (abs(fit$tau - 0.5) <= threshold), drop = FALSE],
    1,
    \(row) diff(range(row)))
}


### QR* ------------------------------------------------------------------------

dcp_fit.rq_opt <- function(formula, data, alpha_sig, tau) {
  rq <- rq(formula, tau = tau, data = data)

  fit <- list(rq = rq, alpha_sig = alpha_sig)
  class(fit) <- "rq_opt"
  fit
}

dcp_predict.rq_opt <- function(fit, data) {
  pred <- predict(fit$rq, newdata = data)
  ## Make sure `pred' is a `nrow(data)' × `length(fit$tau)' matrix,
  ## even if `nrow(data) == 1' so that pred becomes an atomic vector.
  if (is.null(dim(pred))) {
    t(as.matrix(pred))
  } else {
    pred
  }
}

dcp_bhat.rq_opt <- function(fit, pred) {
  b_grid <- fit$rq$tau[fit$rq$tau <= fit$alpha_sig]
  target_tau <- b_grid + 1 - fit$alpha_sig

  compute_bhat <- function(row) {
    leng <- approx(x = fit$rq$tau, y = row, xout = target_tau, rule = 2)$y - row[1:length(b_grid)]
    b_grid[which.min(leng)]
  }

  apply(pred, 1, compute_bhat)
}

dcp_score.rq_opt <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  b_hat <- dcp_bhat(fit, pred)

  abs(rowMeans(pred <= data[[names(fit$rq$model)[1]]]) - b_hat - (1 - fit$alpha_sig)/2)
}

dcp_leng.rq_opt <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  b_hat <- dcp_bhat(fit, pred)

  compute_leng <- function(row) {
    row[-1][abs(fit$rq$tau - row[1] - (1 - fit$alpha_sig)/2) <= threshold] |>
      range() |>
      diff()
  }

  apply(cbind(b_hat, pred), 1, compute_leng)
}


### GLM-DR ---------------------------------------------------------------------

dcp_fit.dr <- function(formula, data, ys) {
  X_vars <- all.vars(formula[[3]])
  Y_var <- deparse(formula[[2]])
  y <- data[, ..Y_var]
  x <- cbind(1, data[, ..X_vars])

  beta <- sapply(ys, \(the_y) suppressWarnings(glm.fit(x, (y <= the_y),
    family = binomial(link = "logit"))$coefficients))
  fit <- list(beta = beta, ys = ys, X_vars = X_vars, Y_var = Y_var)
  class(fit) <- "dr"
  fit
}

dcp_predict.dr <- function(fit, data) {
  X_vars <- fit$X_vars
  ## Just doing `..fit$X_vars' inside data[…] did not work.
  plogis(as.matrix(cbind(1, data[, ..X_vars])) %*% fit$beta)
}

dcp_score.dr <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  purrr::imap_dbl(data[[fit$Y_var]], \(y, idx) approx(x = fit$ys, y = pred[idx, ], xout = y,
    rule = 2)$y - 0.5) |>
    abs()
}

dcp_leng.dr <- function(fit, data, threshold) {
  apply(dcp_predict(fit, data), 1, \(row) diff(range(fit$ys[abs(row - 0.5) <= threshold])))
}

### IDR ------------------------------------------------------------------------

dcp_fit.idrfit <- function(formula, data, ys, groups = NULL, orders = NULL) {
  X_vars <- all.vars(formula[[3]])
  Y_var <- deparse(formula[[2]])

  if (is.null(groups)) {
    groups <- setNames(rep(1, length(X_vars)), X_vars)
  }

  if (is.null(orders)) {
    orders <- c("comp" = 1)
  }

  fit <- idr(data[[Y_var]], data[, ..X_vars], groups = groups, orders = orders)
  fit$X_vars <- X_vars
  fit$Y_var <- Y_var
  fit$ys <- ys
  fit
}

dcp_predict.idrfit <- function(fit, data) {
  predict(fit, data)
}

dcp_score.idrfit <- function(fit, data) {
  abs(pit(dcp_predict(fit, data), data[[fit$Y_var]]) - 0.5)
}

dcp_leng.idrfit <- function(fit, data, threshold) {
  cdf(dcp_predict(fit, data), fit$ys) |>
    apply(1, function(row) diff(range(fit$ys[abs(row - 0.5) <= threshold])))
}


### IDR* ---------------------------------------------------------------------------------

dcp_fit.idrfit_opt <- function(formula, data, alpha_sig, ys, tau, groups = NULL, orders = NULL) {
  X_vars <- all.vars(formula[[3]])
  Y_var <- deparse(formula[[2]])

  if (is.null(groups)) {
    groups <- setNames(rep(1, length(X_vars)), X_vars)
  }

  if (is.null(orders)) {
    orders <- c("comp" = 1)
  }

  fit <- idr(data[[Y_var]], data[, ..X_vars], groups = groups, orders = orders)
  fit <- list(idrfit = fit, alpha_sig = alpha_sig, ys = ys, tau = tau, X_vars = X_vars, Y_var = Y_var)
  class(fit) <- "idrfit_opt"
  fit
}

dcp_predict.idrfit_opt <- function(fit, data) {
  predict(fit$idrfit, data)
}

dcp_bhat.idrfit_opt <- function(fit, pred) {
  b_grid <- fit$tau[fit$tau <= fit$alpha_sig]
  target_tau <- b_grid + 1 - fit$alpha_sig

  apply(qpred(pred, target_tau) - qpred(pred, b_grid), 1, \(row) b_grid[which.min(row)])
}

dcp_score.idrfit_opt <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  abs(pit(pred, data[[fit$Y_var]]) - dcp_bhat(fit, pred) - (1 - fit$alpha_sig)/2)
}

dcp_leng.idrfit_opt <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  cdf <- cdf(pred, fit$ys)
  b_hat <- dcp_bhat(fit, pred)

  compute_leng <- function(row) {
    fit$ys[abs(row[-1] - row[1] - (1 - fit$alpha_sig)/2) <= threshold] |>
      range() |>
      diff()
  }

  apply(cbind(b_hat, cdf), 1, compute_leng)
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
  model_sig <- lm(reformulate(response = "abs(residuals(model_reg))", termlabels = all.vars(formula[[3]])),
    data = data)
  model <- list(reg = model_reg, sig = model_sig)
  class(model) <- "cp_loc"  # I have succumb to the dark side---they had cookies.
  model  # Is a list of two `lm's
}

dcp_predict.cp_loc <- function(fit, data) {
  list(reg = predict(fit$reg, data), sig = predict(fit$sig, data))
}

dcp_score.cp_loc <- function(fit, data) {
  y_name <- names(fit$reg$model)[1]
  pred <- dcp_predict(fit, data)
  abs(pred$reg - data[[y_name]])/abs(pred$sig)
}

dcp_leng.cp_loc <- function(fit, data, threshold) {
  pred <- dcp_predict(fit, data)
  2 * abs(pred$sig) * threshold
}

