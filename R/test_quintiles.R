#' @title Test Significance of Effect Difference Across Quintiles
#' @description plot_quintiles This function exemplifies a Monte-Carlo simulation-based approach for determining if the difference in treatment effect between risk quintiles is statistically significant.
#' @param result The output of the "sweetspot" wrapper function
#' @param nsim The number of simulations to use in calculating the p-value.
#' @return The computed p-value
#' @examples
#' \dontrun{
#'   # Assuming `result` contains model output and match data
#'   pval <- quintile_test(result, nsim = 100)
#'   print(pval)
#' }
#' @include split_quintiles.R
#' @author Danny Del Rosso, Maksim Helmann
#' @export

test_quintiles <- function(result, nsim = 10000){

  if(!all(result$matches[, "treatment_effect"] %in% c(-1, 0, 1))){
    stop("This method should only be used with a binary outcome and one-to-one control-treated matching")
  }

  quintiles <- split_quintiles(result)

  size <- length(quintiles$q1)
  # Replaces negative treatment effects with zeroes to increase robustness to spurious effects
  make_zeroes <- function(ates){
    ates[ates == -1] <- 0
    return(ates)
  }

  quintiles <- lapply(quintiles, make_zeroes)

  #Calculates a cumulative probability across the 3 quintiles
  total_prob <- mean(c(quintiles$q1, quintiles$q3, quintiles$q5))
  #Calculates a score representing how many observations are consistent with a "sweet spot" in the third quartile
  obs_score <- sum(quintiles$q1 == 0) + sum(quintiles$q3 == 1) + sum(quintiles$q5 == 0)

  #Runs the simulation
  ntrials_geq <- 0
  for(i in 1:nsim){
    counter <- 0

    left_right <- rbinom(2*size, 1, total_prob)
    middle <- rbinom(size, 1, total_prob)

    for(trial in left_right){
      counter = counter + ifelse(trial == 0, 1, 0)
    }
    for(trial in middle){
      counter = counter + ifelse(trial == 1, 1, 0)
    }

    ntrials_geq <- ntrials_geq + ifelse(counter >= obs_score, 1, 0)
  }
  return(ntrials_geq/nsim)
}


