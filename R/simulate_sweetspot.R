sim_sweetspot <- function(n, magnitude, window = c(0.4, 0.6), base_effect = 0.05, true_covs = 10,inc_covs = 10, random = F, multicoll = 0.5){

  covariates <- matrix(rnorm(n * true_covs), nrow = n, ncol = true_covs)

  noise <- matrix(rnorm(n * 3, mean = 0, sd = 1), nrow = n, ncol = 3)

  weights <- matrix(runif(true_covs * 3, min = -multicoll, max = multicoll), nrow =   true_covs, ncol = 3)

  for (j in 1:true_covs) {
    covariates[, j] <- covariates[, j] +
      noise[, 1] * weights[j, 1] +
      noise[, 2] * weights[j, 2] +
      noise[, 3] * weights[j, 3]
  }

  treated <- sample(c(0, 1), n, replace = TRUE)

  beta <- runif(true_covs, -1.5, 1)

  bin_indices <- seq(3, true_covs, by = 3)
  for (idx in bin_indices) {
    splitpoint <- median(covariates[, idx])
    covariates[, idx] <- ifelse(covariates[, idx] >= splitpoint, 1, 0)
  }


  if (random == T){
    p <- runif(n, 0, 1)
  } else{
    p <- 1 / (1 + exp(-(covariates %*% beta)))
  }

  in.sweet.spot <- !is.na(cut(p, window))

  p_unadjusted_out <- p[treated == 1 & !in.sweet.spot]
  p_unadjusted_in <- p[treated == 1 & in.sweet.spot]

  p[treated == 1 & !in.sweet.spot] <- p_unadjusted_out + base_effect
  p[treated == 1 &  in.sweet.spot] <- p_unadjusted_in + base_effect + magnitude

  true_effect_sweetspot <- mean(pmin(p[treated == 1 &  in.sweet.spot], 1) - p_unadjusted_in)
  true_effect_outside <- mean(pmin(p[treated == 1 &  !in.sweet.spot], 1) - p_unadjusted_out)
  true_magnitude = true_effect_sweetspot - true_effect_outside

  outcome <- rbinom(n, 1, prob = pmin(p, 1))
  inc_covariates <- covariates[, 1:inc_covs]

  return(list(
    treated = treated,
    outcome = outcome,
    p = pmin(p, 1),
    covariates = inc_covariates,
    true_magnitude = true_magnitude
  )
  )
}
#sweetspot()

#set.seed(1998)
#s <- sim_sweetspot(1000, 0.3, base_effect = 0.1)
#result <- sweetspot(s$treated, s$covariates, s$outcome,  family = "binomial")
#plot_sweetspot(result, "sweetspot")
