## Packages --------------------------------------------------------------------
library(broom)
library(data.table)
library(parallel)
library(stringr)
library(tidyr)

source("dcp.R")


## Simulation ------------------------------------------------------------------

rmodel <- function(n, model, x) {
  ## Generate `n' data points Y | X ~ model(x) conditioned on `x'.
  ## Some values for `model' only make sense if 0 <= x <= 10.
  switch(model,
    "Linear" = 2 * x + rnorm(n),
    "Normal" = rnorm(n, x, 1),
    "D" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) + 10 * (x >= 5),
    "P" = rpois(n, pmin(pmax(x, 1), 6)),
    "NI" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 * (x > 7),
    "S" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)),
    "AR(S)" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)),
    "AR(NI)" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6)) - 2 * (x > 7),
    "AR(P)" = rpois(n, pmin(pmax(x, 1), 6)),
    "Uniform" = runif(n),
    "S1(Uniform)" = runif(n, 1 + x / 10, 2 + 2 * x / 10),
    "S1(Beta)" = {
      dbeta_S1 <- function(y) (dbeta(y, shape1 = 5, shape2 = 2 - x / 20) + 0.01) / 1.01
      ## Do rejection sampling to draw from this density
      n_rejection_sampling <- 100000
      ## Use Unif(0, 1) as proposal distribution.  Note that both Unif(0, 1)
      ## and Beta(5, 2 - x / 10) have support [0, 1].
      proposal <- runif(n_rejection_sampling)
      targetDensity <- dbeta_S1(proposal)
      maxDensity <- max(targetDensity, na.rm = TRUE)
      accepted <- runif(n_rejection_sampling) < (targetDensity / maxDensity)

      sample(proposal[accepted], size = n) # Take random subsample of the accepted samples.
    },
    "S1(Irwin Hall)" = replicate(n, sum(runif(10 + floor(x)))),
    "S1(Bound above)" = rbeta(n, shape1 = 5, shape2 = 2 - x / 20),
    "S1(Bound above shifted)" = x / 10 + rbeta(n, shape1 = 5, shape2 = 2 - x / 10),
    "S1(No bounds)" = rgamma(n, shape = sqrt(x), scale = pmin(pmax(x, 1), 6))
  )
}


generate_data <- function(n_train, n_valid, n_test, model) {
  ## Return a `data.table' with two columns, `X' and `Y'
  ## and `n_train' + `n_valid' + `n_test' rows.
  ## `X' is drawn iid from Unif(0, 10), unless `model' ∈ {AR(1), AR(2)}.
  ## In that case, X[i] depends on on X[i-1] (and X[i-2])
  ## for i = 1, ..., n_train + n_valid and all X values for the test set are
  ## drawn iid depending on X[n_train+n_valid] (and X[n_train+n_valid-1]).
  ## `Y' is drawn conditional on `X', according to `model'.
  n <- n_train + n_valid + n_test
  if (str_starts(model, "AR")) {
      X <- numeric(n)
      steps <- runif(n - 1, -1, 1)
      X[1] <- runif(1, 0, 10)
      for (i in 2:(n_train+n_valid)) {
        X[i] <- 0.95 * X[i-1] + steps[i - 1]
      }
      for (i in (n_train+n_valid+(1:n_test))) {
        X[i] <- 0.95 * X[n_train+n_valid] + steps[i - 1]
      }
      X <- X + 5
      X <- pmin(pmax(X, 0), 10)
      data.table(X = X)[, Y := rmodel(n, model, X)][]
  } else {
    data.table(X = runif(n, 0, 10))[, Y := rmodel(n, model, X)][]
  }
}


make_simulation <- function(runs, alpha_sig, dir = NULL) {
  ## Return a function with three arguments: `n`, `model' and `method'.
  ## The returned function performs a simulation where,
  ## - `n' is the size of the training+validation set (the test set always has size 4096),
  ## - `model' specifies how the data should be generated,
  ## - `method' specifies the DCP method to use (eg, DR, IDR, QR, …)
  ## Optionally, if `results_dir' is not NULL, the simulation results are saved there.

  ## Variables and functions used in a simulation ------------------------------

  num_cores <- detectCores() - 1

  noise <- function(n) {
    runif(n, -1e-6, 1e-6)
  }

  summarise_runs <- function(dt) {
    dt[,
      list(
        ## Estimate the unconditional coverage as the overall mean.
        coverage = mean(conditional_coverage, na.rm = TRUE),
        ## Similarly for the length.
        leng = mean(conditional_leng, na.rm = TRUE),
        ## Summarise the conditional coverage as a logistic regression model
        conditional_coverage = {
          .(tidy(glm(conditional_coverage ~ X, .SD, family = binomial(link = "logit"))))
        },
        ## Summarise the conditional length by binning it according to X
        conditional_leng = {
          .(.SD[,
            .(leng = mean(conditional_leng, na.rm = TRUE)),
            keyby = list(bin = cut(X,
              breaks = seq(floor(min(X)), ceiling(max(X)), length.out = 21),
              include.lowest = TRUE,
              ordered_result = TRUE))])
        }),
      ]
  }


  function(model, method, n) {
    message("================================================================================")
    message(str_c(model, method, n, sep = " "))

    ## Setup
    set.seed(42 + n) # Ensure we generate the same data for every `n'

    n_train <- n / 2
    n_valid <- n / 2
    n_test <- 8192

    ## Function to perform a single run.
    ## This should be fairly fast, as it gets called /a lot/.
    single_run <- function() {
      dt <- generate_data(n_train, n_valid, n_test, model)

      data_train <- dt[1:n_train]
      data_valid <- dt[(n_train+1):(n_train+n_valid)]
      data_test <- dt[(n_train+n_valid+1):(n_train+n_valid+n_test)]

      ## Add noise to training data
      data_train[, X := X + noise(n_train)]
      data_train[, Y := Y + noise(n_train)]

      ## Returns `data.table' with 3 columns:
      ## - X = { Xₙ₊₁⁽ʲ⁾ : j = 1 … n_test }.
      ## - conditional_coverage { 1[Yₙ₊₁⁽ʲ⁾ ∈ C(Xₙ₊₁⁽ʲ⁾)] : j = 1, …, n_test }.
      ## - conditional_leng { |C(Xₙ₊₁⁽ʲ⁾)| : j = 1, …, n_test } a list of estimates for C(Xₙ₊₁).
      dcp(dcp_method = method, formula = Y ~ X, data_train = data_train, data_valid = data_valid, data_test = data_test, alpha_sig = alpha_sig)
    }

    ## Sometimes, …, sometimes the code below fails.
    ## I don't know why.
    ## And I could not figure it out.
    ## So I decided to use the obvious solution:
    ## I just execute the code multiple times.
    n_tries <- 10
    for (try in 1:n_tries) {
      message(str_c("Attempt: ", try))
      tryCatch({
        start <- Sys.time()

        ## Perform the different runs in parallel.
        dt <- mclapply(1:runs, \(x) single_run(), mc.cores = num_cores, mc.silent = TRUE) |>
          rbindlist()

        ## Summarise the results of a simulation.
        dt <- summarise_runs(dt)

        elapsed <- difftime(Sys.time(), start, units = "secs")
        message(str_c("Time:", format(round(elapsed, 3)), sep = " "))

        if (!is.null(dir)) {
          ## Save summarised results to file.
          filename <- file.path(dir, str_c(str_c(method, model, n, sep = "_"), ".rds"))
          saveRDS(dt, file = filename)
        }

        message("================================================================================")
        ## Return results.
        return(dt)
      },
      error = function(cond) {}
      )
    }
    message(str_c("Error in", model, method, n, sep = " "))
    message(str_c("Failed", n_tries, "times", sep = " "))
    data.table(coverage = NaN, leng = NaN, conditional_coverage = list(NA),conditional_leng = list(NA))
  }
}


run_experiment <- function(model, method, n, runs = 500, alpha_sig = 0.1, sub_dir = NULL) {
  ## Create directory to save results in.
  dir <- file.path("results",
    if(is.null(sub_dir)) "tmp" else sub_dir,
    format(Sys.time(), "%Y%m%d%H%M%S"))
  dir.create(dir, recursive = TRUE)

  run_simulation <- make_simulation(runs, alpha_sig, dir)
  start <- Sys.time()
  dt <- as.data.table(expand_grid(model = model, method = method, n = n))[
    order(n, method, model)][,
    c("coverage", "leng", "conditional_coverage", "conditional_leng")
    := run_simulation(model, method, n),
    by = .I][]
  elapsed <- difftime(Sys.time(), start, units = "hours")
  message(str_c("Overall time:", format(round(elapsed, 3)), sep = " "))

  saveRDS(dt, file = file.path(dir, str_c("result", format(Sys.time(), "%H%M%S"), ".rds")))

  dt
}


stitch_results <- function(dir) {
  current_dir <- getwd()
  on.exit(setwd(current_dir))
  setwd(dir)

  result <- lapply(list.files(), \(file) {
    match <- str_match(file, "((?:CP_LOC)|(?:CP_OLS)|[A-Z*]+)_(.*?)_([0-9]+)\\.rds")
    if (!(is.na(match[1]))) {
      method <- match[1, 2]
      model <- match[1, 3]
      n <- match[1, 4] |> as.integer()
      result <- readRDS(file)
      result[, `:=`(method = ..method, model = ..model, n = ..n)][]
    }
  }) |>
    rbindlist()

  setcolorder(result, c("model", "method", "n", "coverage", "leng", "conditional_coverage", "conditional_leng"))
  setorder(result, n, method, model)
  saveRDS(result, file = file.path(str_c("result_stitched", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds")))
  result
}
