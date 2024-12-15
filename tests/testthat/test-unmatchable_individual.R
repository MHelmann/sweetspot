test_that("compute_avg_treatment_effect works correctly", {
  set.seed(123)  # For reproducibility

  treated <- sample(c(0, 1), size = 500, replace = TRUE)
  risk_scores <- runif(500, min = 0, max = 1)
  scaled_effect <- 1 / (1+exp(-risk_scores)) # probability
  control_treated_ratio <- 1
  response <- sample(c(0, 1), size = 500, replace = TRUE)
  matched_sets <- create_match_sets(treated, risk_scores, control_treated_ratio)
  result <- compute_avg_treat_effect(matched_sets$matched_sets, matched_sets$patients_df, risk_scores, scaled_effect, control_treated_ratio, response)

  expect_true(is.matrix(result), info = "Result should be a matrix")

  expect_equal(nrow(result), length(unique(result[, "mean_score"])),
               "Number of rows should match unique matched groups")
})

test_that("create_match_sets handles non-binary treated vector", {
  # Invalid treated vector
  treated <- c(0, 1, 2, 1, 0)
  risk_scores <- runif(5, min = 0, max = 1)
  control_treated_ratio <- 1

  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio),
    "The 'treated' vector must be binary"
  )
})



test_that("create_match_sets handles non-integer control_treated_ratio", {
  # Invalid control_treated_ratio
  treated <- c(1, 0, 1, 0, 1)
  risk_scores <- runif(5, min = 0, max = 1)
  control_treated_ratio <- 1.5  # Not an integer

  # Expect an error
  expect_error(
    create_match_sets(treated, risk_scores, control_treated_ratio),
    "'control_treated_ratio' must be a positive integer"
  )
})
