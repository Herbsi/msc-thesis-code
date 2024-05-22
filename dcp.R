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
    fit <- dcp_fit(type, formula, data_train)
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

dcp_fit <- function(type, formula, data) {
  switch(type,
    "QR" = dcp_fit.rqs(formula, data),
    "DR" = dcp_fit.dr(formula, data),
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
dcp_fit.rqs <- function(formula, data) {
  rq(formula, tau = seq(0.001, 0.999, length = 200) , data = data)
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

