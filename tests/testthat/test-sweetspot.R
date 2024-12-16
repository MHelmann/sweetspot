#This tests the dependencies as well

treated <- rbinom(100, 1, 0.5)
covariates <- matrix(rnorm(1000), nrow = 100, ncol = 10)
response <- rbinom(100, 1, 0.5)
result <- sweetspot(treated, covariates, response, family = "binomial")

test_that("sweetspot returns a sweetspot object", {
  expect_true(inherits(result, "sweetspot_object"))
})

test_that("plot_sweetspot returns a ggplot object", {
  p1 <- plot_sweetspot(result, "title")
  expect_true(inherits(p1, "ggplot"))
})

test_that("split_quintiles returns a list", {
  sq <- split_quintiles(result)
  expect_true(is.list(sq))
})

test_that("plot_quintiles returns a ggplot object", {
  p2 <- plot_quintiles(result, "title")
  expect_true(inherits(p2, "ggplot"))
})

test_that("test_quintiles returns a valid p-value", {
  pval <- test_quintiles(result, nsim = 100)
  expect_true(pval >= 0 && pval <= 1)
})
