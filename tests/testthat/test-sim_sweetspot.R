test_that("sim_sweetspot produces binary data", {
  s <- sim_sweetspot(100, 0, window = c(0.1, 0.2), base_effect = 0.1, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F)
  expect_true(all(s$outcome %in% c(0, 1)))
  expect_true(all(s$treated %in% c(0, 1)))
  expect_true(all(!is.na(s$covariates)))
  expect_equal(dim(s$covariates), c(100, 4))
})

test_that("sim_sweetspot properly handles negative multicollinearity values", {
  expect_error(sim_sweetspot(100, 0, window = c(0.1, 0.2), base_effect = 0.1, true_covs = 4,inc_covs = 4, random = F, multicoll = -1, xbin = F))
})

test_that("sim_sweetspot properly handles incorrectly specified covariates", {
  expect_error(sim_sweetspot(100, 0, window = c(0.1, 0.2), base_effect = 0.1, true_covs = 1,inc_covs = 4, random = F, multicoll = -1, xbin = F))
})

test_that("sim_sweetspot properly handles incorrectly specified treatment windows", {
  expect_error(sim_sweetspot(100, 0, window = c(-0.1, 0.2), base_effect = 0.1, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F))
})

test_that("sim_sweetspot properly handles incorrectly specified base effects", {
  expect_error(sim_sweetspot(100, 0, window = c(0.1, 0.2), base_effect = 1.2, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F))
})

test_that("sim_sweetspot properly handles incorrectly specified sweetspot effects", {
  expect_error(sim_sweetspot(100, -0.1, window = c(0.1, 0.2), base_effect = 0.9, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F))
})


