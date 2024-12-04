test_that("risk_scores works with binary response and binomial family", {
  set.seed(123)

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
  set.seed(123)

  treated <- rbinom(100, 1, 0.5)
  covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  response <- rnorm(100)

  # Test invalid family
  expect_error(risk_scores(
    treated = treated,
    covariates = covariates,
    response = response,
    family = "invalid_family"
  ))


})

test_that("risk_scores works with regularized penalties", {
  set.seed(123)

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
