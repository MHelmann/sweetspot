#' @title Simulate Continuous Sweetspot Data
#' @description This function generates continuous data compatible with the package, for a user-specified sweetspot.
#' @param n A positive integer representing the amount of observations to be generated.
#' @param magnitude The additional increase in the expected value of the continuous outcome caused by the treatment in the sweetspot.
#' @param window A 2-element numeric vector with the boundaries of the outcome values that define the sweetspot. Default is `c(-1, 1)`.
#' @param base_effect The increase in the expected value of the continuous outcome caused by the treatment outside of the sweetspot. Default is `0.05`.
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
#' }
#' @author Danny Del Rosso, Maksim Helmann
#' @export
#'
sim_norm_sweetspot <- function(n, magnitude, window = c(-1, 1), base_effect = 0.2, true_covs = 10,inc_covs = 10, random = F, multicoll = 0, xbin = T){

  if (multicoll < 0 ) {
    stop("multicollinearity value must be positive")
  }
  else if (multicoll > 1 ) {
    warning("multicollinearity between covariates is likely too high for meaningful results")
  }

  if(true_covs < inc_covs){
    stop("true_covs should be greater than or equal to the number of included covariates")
  }

  if(magnitude < 0){
    stop("Sweetspot is not currently programmed to detect negative or weaker effects")
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

  e <- rnorm(n, 0.25)
  outcome <- E_y + e
  inc_covariates <- covariates[, 1:inc_covs]

  return(list(
    treated = treated,
    outcome = outcome,
    E_y = E_y,
    covariates = inc_covariates
  )
  )
}
