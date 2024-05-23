library(isodistrreg)
library(quantreg)

dcp <- function(type, formula, data, split, alpha = 0.1) {
  ## TODO <2024-05-22 Wed> Agree on interface for train-valid-test split
  with(split(data), {
    data_train <- train
    data_valid <- valid
    data_test <- test
    print("Data split.")
    
    ## Fit model
    tau <- seq(0.001, 0.999, length = 200)
    ys <- quantile(unique(c(data_train$Y, data_valid$Y)), tau)
    
    fit <- dcp_fit(type, formula, data_train, tau = tau, ys = ys)
    print("Model fitted.")

    ## Calibrate model
    scores_valid <- dcp_score(fit, data_valid)
    print("Scores calculated.")
    fit$threshold <- sort(scores_valid)[ceiling((1 - alpha) * (1 + length(scores_valid)))]
    print("Threshold calculated.")

    ## Estimate coverage
    coverage <- dcp_score(fit, data_test) <= fit$threshold
    print("Coverage estimated.")
    leng <- dcp_leng(fit, data_test)
    print("Interval length estimated.")

    data.frame(coverage = coverage, leng = leng)
  })
}    

dcp_fit <- function(type, formula, data, ...) {
  args <- list(...)
  switch(type,
    "QR" = dcp_fit.rqs(formula, data, args$tau),
    "DR" = dcp_fit.dr(formula, data, args$ys),
    "QR*" = dcp_fit.rq_opt(formula, data),
    "IDR" = dcp_fit.idrfit(formula, data),
    "CP-OLS" = dcp_fit.lm(formula, data),
    "CP-LOC" = dcp_fit.cp_loc(formula, data)
  )
}


dcp_predict <- function(fit, data) {
  UseMethod("dcp_predict")
}
dcp_score <- function(fit, data) {
  UseMethod("dcp_score")
}
dcp_leng <- function(fit, data) {
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

dcp_leng.rqs <- function(fit, data) {
  ## Assumes `fit' has its `threshold'
  leng <- apply(dcp_predict(fit, data)[, (abs(fit$tau - 0.5) <= fit$threshold)],
    1,
    \(row) max(row) - min(row)
  )
  leng[which(leng == -Inf)] <- NA
  leng 
}


### GLM-DR ---------------------------------------------------------------------

dcp_fit.dr <- function(formula, data, ys) {
  ## FIXME <2024-05-23 Thu> This does not use validation Ys
  y <- data[[formula[[2]]]]
  x <- cbind(1, data[, formula[[3]]])
  ## TODO <2024-05-23 Thu> Use something other than `sapply'
  beta <- sapply(ys,
    \(the_y) glm.fit(x, (y <= the_y), family = binomial(link = "logit"))$coefficients)
  fit <- list(beta = beta, ys = ys)
  class(fit) <- "dr"
  fit
}

dcp_predict.dr <- function(fit, data) {
  ## HACK <2024-05-23 Thu> Hard-coded `$X' here
  plogis(as.matrix(cbind(1, data[, "X"])) %*% fit$beta)
}

dcp_score.dr <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  ## HACK <2024-05-23 Thu> Hard-coded `$Y' here
  imap_dbl(data$Y,
    \(y, idx) approx(x = fit$ys, y = pred[idx, ], xout = y, rule = 2)$y
    - 0.5) |>
    abs()
}

dcp_leng.dr <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  leng <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    tmp <- fit$ys[abs(pred[i, ] - 0.5) <= fit$threshold]
    leng[i] <- max(tmp) - min(tmp)
  }
  leng[which(leng == -Inf)] <- NA
  leng
}

### IDR ------------------------------------------------------------------------

dcp_fit.idrfit <- function(formula, data) {
  ## NOTE <2024-05-22 Wed> Extracting the formula could be more robust.
  y <- data[[formula[[2]]]]
  x <- data[, formula[[3]]]
  idr(y, x)
}

dcp_predict.idrfit <- function(fit, data) {
  predict(fit, data)
}

dcp_score.idrfit <- function(fit, data) {
  ## HACK <2024-05-22 Wed> `$Y' is hard-coded here,
  ## but the IDR package is not the most robust.
  abs(pit(dcp_predict(fit, data), data$Y) - 0.5)
}

dcp_leng.idrfit <- function(fit, data) {
  leng <- dcp_predict(fit, data) |>
    map_dbl(~ {
      tmp <- .x$points[abs(.x$cdf - 0.5) <= fit$threshold]
      max(tmp) - min(tmp)
    }
    )
  leng[which(leng == -Inf)] <- NA
  leng
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

dcp_leng.lm <- function(fit, data) {
  rep(2 * fit$threshold, nrow(data))
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

dcp_leng.cp_loc <- function(fit, data) {
  pred <- dcp_predict(fit, data)
  2 * pred$sig * fit$threshold
}

