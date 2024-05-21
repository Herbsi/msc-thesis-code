################################################################################
## Reimplementations of distributional regression methods
################################################################################

library(isodistrreg)
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

tau = seq(0.001, 0.999, length = 200)

#### Quantile regression
dcp.qr <- function(data.train, data.valid, data.test, alpha) {
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


#### DCP-QR*
dcp.opt <- function(data.train, data.valid, data.test, alpha){
  Y0 <- data.train$Y
  X0 <- as.matrix(data.train$X)
  Y1 <- data.valid$Y
  X1 <- as.matrix(data.valid$X)
  Y.test <- data.test$Y
  X.test <- as.matrix(data.test$X)
  alpha.sig <- alpha
  taus <- tau
  
  ## START PLAGARISM
  XXX <- rbind(X1,X.test)
  YYY <- c(Y1,Y.test)
  
  beta.qr <- matrix(NA,dim(X0)[2]+1,length(taus))
  for (t in 1:length(taus)){
    beta.qr[,t] <- rq.fit.br(cbind(1,X0),Y0,tau=taus[t])$coefficients
  }
  tQ.yx   <- cbind(1,XXX)%*%beta.qr
  Q.yx    <- t(apply(tQ.yx,1,FUN=sort))
  u.hat <- rowMeans((Q.yx <= matrix(YYY,length(YYY),dim(beta.qr)[2])))
  
  bhat <- rep(NA,length(YYY))
  b.grid <- taus[taus<=alpha.sig]
  for (t in 1:length(YYY)){
    leng <- rep(NA,length(b.grid))
    leng.test <- rep(NA,length(b.grid))
    for (b in 1:length(b.grid)){
      Q.yx.u <- approx(x=taus,y=Q.yx[t,],xout=(b.grid[b]+1-alpha.sig),rule=2)$y
      leng[b] <- Q.yx.u -Q.yx[t,b]
    }
    bhat[t] <- b.grid[which.min(leng)]
  }
  
  ind.test <- (length(Y1)+1):length(YYY)
  
  cs.opt <- abs(u.hat-bhat-(1-alpha.sig)/2)
  
  k           <- ceiling((1-alpha.sig)*(1+length(Y1)))
  threshold   <- sort(cs.opt[-ind.test])[k]
  
  cov.opt   <- (cs.opt[ind.test] <= threshold)
  
  leng.opt <- NULL
  for (t in ind.test){
    ci.grid <- abs(taus - bhat[t]-(1-alpha.sig)/2)
    ci <- Q.yx[t,(ci.grid <= threshold)]
    ub <- max(ci)
    lb <- min(ci)
    leng.opt <- c(leng.opt,ub-lb)
  }
  
  leng.opt[which(leng.opt==-Inf)] <- NA
  
  return(list(coverage=cov.opt, leng=leng.opt))  
  ## END PLAGARISM
}


#### GLM-based DR
dcp.dr <- function(data.train, data.valid, data.test, alpha) {
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

dcp.idr <- function(data.train, data.valid, data.test, alpha) {
  fm <- idr(data.train$Y, data.train[, "X"])

  local.score <- function(data, pred = predict(fm, data[, "X"])) {
    return(dcp.score(pit(pred, data$Y)))
  }
  
  ## Scores on calibration set
  threshold <- dcp.threshold(local.score(data.valid), alpha)

  ## Scores on test set
  pred.test <- predict(fm, data.test[, "X"])
  
  coverage <- local.score(data.test, pred.test) <= threshold

  ## Calculate length of interval(s)
  leng <- rep(NA, nrow(data.test))
  for (i in 1:nrow(data.test)) {
    ## NOTE <2024-05-19 Sun> This is inspired by the length calculation for `dcp.dr', but I don't fully understand how it works yet.
    ys.i <- quantile(unique(c(data.train$Y, data.valid$Y)), seq(0.001, 0.999, length=nrow(pred.test[[i]])))
    
    indices <- as_tibble(pred.test[[i]]) |>
      mutate(indices = dcp.score(cdf) <= threshold, .keep = "unused") |>
      pull(indices)

    tmp <- ys.i[indices]
    leng[i] <- max(tmp) - min(tmp)
  }

  leng[which(leng == -Inf)] <- NA
  
  return(list(coverage = coverage, leng = leng))
}


#### CP-OLS

cp.ols <- function(data.train, data.valid, data.test, alpha) {
  model <- lm(Y ~ X, data = data.train)

  local.score <- function(data, pred = predict(model, newdata = data)) {
    ## OLS-based CP just uses the absolute value of the residuals.
    return(abs(data$Y - pred))
  }

  ## Calculate threshold based on validation set and calculate coverage on test set
  threshold <- dcp.threshold(local.score(data.valid), alpha)
  
  pred.test <- predict(model, newdata = data.test)

  ## Estimate coverage as test values within threshold
  coverage <- local.score(data.test, pred.test) <= threshold

  leng <- rep(2 * threshold, nrow(data.test))

  return(list(coverage = coverage, leng = leng))
}


#### CP-loc
## NOTE <2024-05-21 Tue> I don't know what this method does yet.

cp.loc <- function(data.train, data.valid, data.test, alpha) {
  model.reg <- lm(Y ~ X, data = data.train)

  model.sig <- lm(abs(residuals(model.reg)) ~ X, data = data.train)

  local.score <- function(data, pred = predict(model.reg, newdata = data)) {
    return(abs(data$Y - pred) / abs(predict(model.sig, newdata = data)))
  }

  threshold <- dcp.threshold(local.score(data.valid), alpha)

  lb <- predict(model.reg, newdata = data.test) - threshold * abs(predict(model.sig, data.test))
  ub <- predict(model.reg, newdata = data.test) + threshold * abs(predict(model.sig, data.test))

  coverage <- data.test$Y <= ub & data.test$Y >= lb
  leng <- ub - lb

  return(list(coverage = coverage, leng = leng))
}
