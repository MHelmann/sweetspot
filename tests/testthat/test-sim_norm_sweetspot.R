test_that("sim_norm_sweetspot produces functional data", {
  s <- sim_norm_sweetspot(100, 0, window = c(-1, 1), base_effect = 1, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F)
  expect_true(all(!is.na(s$covariates)))
  expect_true(all(!is.na(s$outcome)))
  expect_true(all(!is.na(s$treated)))
  expect_equal(dim(s$covariates), c(100, 4))
})

test_that("sim_norm_sweetspot properly handles negative multicollinearity values", {
  expect_error(sim_norm_sweetspot(100, 0, window = c(-1, 1), base_effect = 1, true_covs = 4,inc_covs = 4, random = F, multicoll = -1, xbin = F))
})


test_that("sim_norm_sweetspot properly handles incorrectly specified covariates", {
  expect_error(sim_sweetspot(100, 0, window = c(-1, 1), base_effect = 1, true_covs = 1,inc_covs = 4, random = F, multicoll = 1, xbin = F))
})

test_that("sim_norm_sweetspot properly handles incorrectly specified sweetspot effects", {
  expect_error(sim_norm_sweetspot(100, -1, window = c(-1, 1), base_effect = 1, true_covs = 4,inc_covs = 4, random = F, multicoll = 1, xbin = F))
})
