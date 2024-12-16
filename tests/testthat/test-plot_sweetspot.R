test_that("plot_sweetspot returns a valid plot", {
  set.seed(1998)
  s <- sim_sweetspot(100, 0.3, base_effect = 0.1)
  result <- sweetspot(s$treated, s$covariates, s$outcome,  family = "binomial")
  p <- plot_sweetspot(result, "title")
  expect_true(inherits(p, "ggplot"))
})


