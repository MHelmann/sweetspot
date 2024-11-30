sim_sweetspot_normal <- function(n, magnitude, window = c(-1, 1), base_effect = 0.2, true_covs = 10,inc_covs = 10, random = F){

  covariates <- matrix(rnorm(n * true_covs), nrow = n, ncol = true_covs)

  noise <- matrix(rnorm(n * 3, mean = 0, sd = 1), nrow = n, ncol = 3)

  weights <- matrix(runif(true_covs * 3, min = -0.5, max = 0.5), nrow =   true_covs, ncol = 3)

  for (j in 1:true_covs) {
    covariates[, j] <- covariates[, j] +
      noise[, 1] * weights[j, 1] +
      noise[, 2] * weights[j, 2] +
      noise[, 3] * weights[j, 3]
  }
  treated <- sample(c(rep(0, ceiling(n/2)), rep(1, floor(n/2))), replace = FALSE)

  beta <- runif(true_covs, -1, 1)

  bin_indices <- seq(3, true_covs, by = 3)
  for (idx in bin_indices) {
    splitpoint <- median(covariates[, idx])
    covariates[, idx] <- ifelse(covariates[, idx] >= splitpoint, 1, 0)
  }


  if (random == T){
    E_y <- rnorm(n, 0, 2)
  } else{
    E_y <- covariates %*% beta

  }

  in.sweet.spot <- !is.na(cut(E_y, window))

  E_y_unadjusted_out <- E_y[treated == 1 & !in.sweet.spot]
  E_y_unadjusted_in <- E_y[treated == 1 & in.sweet.spot]

  E_y[treated == 1 & !in.sweet.spot] <- E_y_unadjusted_out + base_effect
  E_y[treated == 1 &  in.sweet.spot] <- E_y_unadjusted_in + base_effect + magnitude

  true_effect_sweetspot <- mean(E_y[treated == 1 &  in.sweet.spot] - E_y_unadjusted_in)
  true_effect_outside <- mean(E_y[treated == 1 &  !in.sweet.spot] - E_y_unadjusted_out)
  true_magnitude = true_effect_sweetspot - true_effect_outside

  e <- rnorm(n, 0.25)
  outcome <- E_y + e
  inc_covariates <- covariates[, 1:inc_covs]

  return(list(
    treated = treated,
    outcome = outcome,
    E_y_unadjusted_in = E_y_unadjusted_in,
    covariates = inc_covariates,
    true_magnitude = true_magnitude
  )
  )
}

set.seed(1998)
dnorm <- sim_sweetspot_normal(1000, 1)

#$model$mean_inside
#[1] 1.125288

#$model$mean_outside
#[1] 0.1019634

dnorm <- sim_sweetspot_normal(1000, 0)

#$model$mean_inside
#[1] 0.1415671

#$model$mean_outside
#[1] -0.182534
set.seed(2002)
dnorm <- sim_sweetspot_normal(1000, 0)



#dnorm <- sim_sweetspot_normal(1000, 4) #b
#dnorm <- sim_sweetspot_normal(1000, 10, base_effect = 5)

scores <- risk_scores(dnorm$treated, dnorm$covariates, dnorm$outcome, family = "gaussian", nfolds = 10)

#sweetspot(dnorm$treated, dnorm$covariates, dnorm$outcome, family = "gaussian")
#$model$mean_inside_debiased
#[1] 0.152514

#$model$mean_outside_debiased
#[1] -0.2470645

set.seed(2024)
dnorm <- sim_sweetspot_normal(1000, 0)
#$model$p_value
#[1] 0.16

#$model$mean_inside_debiased
#[1] 0.0555593

#$model$mean_outside_debiased
#[1] -0.1626624


#set.seed(2024)
#dnorm <- sim_sweetspot_normal(1000, -10, base_effect = -5)
#sweetspot(dnorm$treated, dnorm$covariates, dnorm$outcome, family = "gaussian")


#target_value <- 0.490705496

# Find indices where the value matches to 3 decimal points
#indices <- which(abs(scores$risk_scores - target_value) < 0.001)

#scores$model$fit.preval[, 3 + 1]

#scores$risk_scores[dnorm$treated == 0]


#hist(dnorm$E_y_unadjusted_in)
#result <- sweetspot(dnorm$treated, dnorm$covariates, dnorm$outcome, family = "gaussian")
#result$risk_score_model$glmnet.fit$dev.ratio
