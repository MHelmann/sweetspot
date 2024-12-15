
# Test 1: Handles uniform treatment effect
test_that("find_sweetspot handles uniform treatment effect", {
  treatment_effect <- rep(0.5, 20)
  
  result <- find_sweetspot(treatment_effect, ntrials_significance = 100, ntrials_bias = 100)
  
  expect_equal(result$statistic, 0)  # No deviation from the global mean
  expect_equal(result$p_value, 1)  # P-value should indicate no significance
})

# Test 2: Handles negative treatment effects
test_that("find_sweetspot works with negative treatment effects", {
  treatment_effect <- c(-0.1, -0.2, -1.5, -1.6, -0.3, -0.2, 0.5, 0.4)
  
  result <- find_sweetspot(treatment_effect, ntrials_significance = 100, ntrials_bias = 100)
  
  expect_true(result$statistic > 0)  # Statistic should be positive
  expect_true(result$p_value > 0 && result$p_value <= 1)  # P-value should be in range (0, 1]
})

# Test 3: Minimum and maximum window size constraints
test_that("find_sweetspot respects window size constraints", {
  treatment_effect <- rnorm(100)
  
  result <- find_sweetspot(
    treatment_effect,
    ntrials_significance = 100,
    ntrials_bias = 100,
    min_size_fraction = 0.1,
    max_size_fraction = 0.3
  )
  
  expect_true(result$end_index - result$start_index + 1 >= 10)  # Minimum window size
  expect_true(result$end_index - result$start_index + 1 <= 30)  # Maximum window size
})




# Test 4: Debiased means are computed correctly
test_that("find_sweetspot computes debiased means", {
  treatment_effect <- rnorm(50, mean = 0.5, sd = 0.1)
  
  result <- find_sweetspot(treatment_effect, ntrials_significance = 50, ntrials_bias = 50)
  
  expect_true(!is.na(result$mean_inside_debiased))
  expect_true(!is.na(result$mean_outside_debiased))
})

# Test 5: Handles random noise
test_that("find_sweetspot handles random noise", {
  set.seed(42)
  treatment_effect <- rnorm(100)
  
  result <- find_sweetspot(treatment_effect, ntrials_significance = 100, ntrials_bias = 100)
  
  expect_true(result$statistic > 0)  # Statistic should be positive even with noise
  expect_true(result$p_value > 0 && result$p_value <= 1)  # P-value should be valid
})

# Test 6: Handles zero treatment effects
test_that("find_sweetspot handles zero treatment effects", {
  treatment_effect <- rep(0, 100)
  
  result <- find_sweetspot(treatment_effect, ntrials_significance = 100, ntrials_bias = 100)
  
  expect_equal(result$statistic, 0)  # No deviation
  expect_equal(result$p_value, 1)  # P-value should indicate no significance
})
