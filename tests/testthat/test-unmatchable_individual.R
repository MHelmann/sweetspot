test_that("Create_match_sets works correctly", {
  set.seed(123)  # For reproducibility

  treated <- sample(c(0, 1), size = 500, replace = TRUE)
  risk_scores <- runif(500, min = 0, max = 1)
  control_treated_ratio <- 1
  response <- sample(c(0, 1), size = 500, replace = TRUE)
  result <- create_match_sets(treated, risk_scores, control_treated_ratio, response)
  
  
  expect_true(is.matrix(result), info = "Result should be a matrix")
  
  expect_equal(nrow(result), length(unique(result[, "mean_score"])), 
               "Number of rows should match unique matched groups")
})

test_that("create_match_sets handles non-binary treated vector", {
  # Invalid treated vector
  treated <- c(0, 1, 2, 1, 0)
  risk_scores <- runif(5, min = 0, max = 1)
  control_treated_ratio <- 1
  response <- c(0, 1, 0, 1, 1)
  
  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio, response),
    "The 'treated' vector must be binary"
  )
})

test_that("create_match_sets handles risk_scores out of range", {
  # Invalid risk_scores
  treated <- c(1, 0, 1, 0, 1)
  risk_scores <- c(1.1, -0.2, 0.5, 0.3, 0.9)  # Contains values out of range
  control_treated_ratio <- 1
  response <- c(0, 1, 0, 1, 1)
  
  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio, response),
    "The parameter must be a numeric vector with values between 0 and 1"
  )
})

test_that("create_match_sets handles non-integer control_treated_ratio", {
  # Invalid control_treated_ratio
  treated <- c(1, 0, 1, 0, 1)
  risk_scores <- runif(5, min = 0, max = 1)
  control_treated_ratio <- 1.5  # Not an integer
  response <- c(0, 1, 0, 1, 1)
  
  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio, response),
    "'control_treated_ratio' must be a positive integer"
  )
})

test_that("create_match_sets handles non-binary response vector", {
  # Invalid response vector
  treated <- c(1, 0, 1, 0, 1)
  risk_scores <- runif(5, min = 0, max = 1)
  control_treated_ratio <- 1
  response <- c(0, 2, 1, 1, 0)  # Contains a value not in {0, 1}
  
  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio, response),
    "'response' must be a numeric vector containing only 0 and 1"
  )
})

