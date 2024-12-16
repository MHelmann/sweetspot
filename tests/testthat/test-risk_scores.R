test_that("risk_scores works with binary response and binomial family", {
  set.seed(100)

  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rbinom(100, 1, 0.5)

  result <- risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "binomial",
    regularized = F,
    nfolds = NULL)

  expect_type(result, "list")
  expect_true(inherits(result$model, "cv.glmnet"))
  expect_equal(length(result$risk_scores), length(response))
  expect_true(all(result$pred_response[!is.na(result$risk_scores)] >= 0 &
                  result$pred_response[!is.na(result$risk_scores)] <= 1))
})



test_that("risk_scores handles invalid input correctly", {
  set.seed(100)

  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  expect_error(risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "negative_binomial"
  ))


})

test_that("risk_scores works with regularized penalties", {
  set.seed(100)

  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rbinom(100, 1, 0.5)

  result <- risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "binomial",
    regularized = TRUE,
    nfolds = 10
  )

  expect_true(result$model$lambda.min <= result$model$lambda.1se)
})


test_that("risk_scores works with poisson response and poisson family", {
  set.seed(100)

  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rpois(100, 5)

  result <- risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "poisson",
    regularized = F,
    nfolds = NULL)

  expect_type(result, "list")
  expect_true(inherits(result$model, "cv.glmnet"))
  expect_equal(length(result$risk_scores), length(response))
  expect_true(all(result$pred_response[!is.na(result$risk_scores)] >= 0))
})


test_that("risk_scores works with continuous response and gaussian family", {
  set.seed(100)
  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  result <- risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "gaussian",
    regularized = F,
    nfolds = NULL)

  expect_type(result, "list")
  expect_true(inherits(result$model, "cv.glmnet"))
  expect_equal(length(result$risk_scores), length(response))
})

test_that("risk_scores is robust to mis-specified data", {
  set.seed(100)
  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  expect_error(risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "binomial",
    regularized = F,
    nfolds = NULL)
  )
})

test_that("risk_scores is robust to mis-specified data as counts", {
  set.seed(100)
  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  expect_error(risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "poisson",
    regularized = F,
    nfolds = NULL)
  )
})


test_that("risk_scores alerts users of poor model fit", {
  set.seed(100)
  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  expect_warning(risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "gaussian",
    regularized = F,
    nfolds = NULL)
  )
})




