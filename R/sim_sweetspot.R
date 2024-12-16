#' @title Simulate Binary Sweetspot Data
#' @description This function generates data compatible with the package, for a user-specified sweetspot.
#' @param n A positive integer representing the amount of observations to be generated.
#' @param magnitude The additional increase in probability of survival caused by the treatment in the sweetspot.
#' @param window A 2-element numeric vector with the boundaries of the predilection probabilities that define the sweetspot. Default is `c(0.4, 0.6)`.
#' @param base_effect The increase in probability of survival caused by the treatment outside of the sweetspot. Default is `0.05`.
#' @param true_covs The number of covariates used to generate the true predilection probabilities. Default is `10`.
#' @param inc_covs The number of covariates included in the model. Default is `10` (i.e. all of the covariates that were used to generate the true probabilities).
#' @param random A boolean indicating if the predilection probabilities should be generated randomly, overriding all previous information about covariates. Default is `FALSE`.
#' @param multicoll A positive number indicating the degree of relatedness of the predictors. Ranges between `0` (no multicollinearity) to `1` (covariates in the model are largely controlled by other common, unseen covariates).
#' @param xbin A boolean indicating if every third covariate should be dichotomized to generate binary covariates.
#' @return A list with the following 5 elements:
#' \describe{
#'   \item{treated}{A binary vector representing control (`0`) or treated (`1`) status}
#'   \item{outcome}{A binary vector representing the presence of an outcome (`1`), here representing a positive outcome.}
#'   \item{p}{The probabilities used to generate the outcome.}
#'   \item{covariates}{A matrix of included covariates, with the rows representing each observation.}
#'   \item{true_magnitude}{The true difference between the treatment effects inside and outside of the sweetspot, corrected for ceiling effects.}
#' }
#' @author Danny Del Rosso, Erin Craig, Maksim Helmann
#' @export

sim_sweetspot <- function(n, magnitude, window = c(0.4, 0.6), base_effect = 0.05, true_covs = 10,inc_covs = 10, random = F, multicoll = 0, xbin = T){

  if (multicoll < 0 ) {
    stop("the multicollinearity value must be positive")
  }
  else if (multicoll > 1 ) {
    warning("multicollinearity between covariates is likely too high for meaningful results")
  }

  if(true_covs < inc_covs){
    stop("true_covs should be greater than or equal to the number of included covariates")
  }

  if(magnitude < 0 || magnitude > 1 || base_effect < 0 || base_effect > 1){
    stop("Inputs for treatment effect should be positive and on the probability scale")
  }

  if(window[1] < 0 || window[1] > 1 || window[2] < 0 || window[2] > 1){
    stop("Inputs for the sweetspot window -should be positive and on the probability scale")
  }


  covariates <- matrix(rnorm(n * true_covs), nrow = n, ncol = true_covs)

  noise <- matrix(rnorm(n * 3, mean = 0, sd = 1), nrow = n, ncol = 3)

  weights <- matrix(runif(true_covs * 3, min = -multicoll, max = multicoll), nrow =   true_covs, ncol = 3)

  for (j in 1:true_covs) {
    covariates[, j] <- covariates[, j] +
      noise[, 1] * weights[j, 1] +
      noise[, 2] * weights[j, 2] +
      noise[, 3] * weights[j, 3]
  }

  treated <- sample(c(rep(0, ceiling(n/2)), rep(1, floor(n/2))), replace = FALSE)

  beta <- runif(true_covs, -1.5, 1)

  bin_indices <- seq(3, true_covs, by = 3)

  if(xbin == T){
    for (idx in bin_indices) {
      splitpoint <- median(covariates[, idx])
      covariates[, idx] <- ifelse(covariates[, idx] >= splitpoint, 1, 0)
    }
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

#set.seed(1998)
#s <- sim_sweetspot(1000, 0.3, base_effect = 0.1, true_covs = 6, o)

#result <- sweetspot(s$treated, s$covariates, s$outcome,  family = "binomial")
#plot_sweetspot(result, "sweetspot", "No sweet spot related to illness severity")
#summary(result)
#plot_quintiles(result, "sweetspot")

#sweetspot()




#result <- sweetspot(s$treated, s$covariates, s$outcome,  family = "binomial")
#summary(result)
#plot_sweetspot(result, "sweetspot", hypothesis = "")
