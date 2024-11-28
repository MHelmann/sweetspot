#' @title Perform a sweet spot analysis on clinical trial data.
#' @description Identifying heterogeneous treatment effects (HTEs) in randomized controlled trials is an important step toward understanding and acting on trial results. However, HTEs are often small and difficult to identify, and HTE modeling methods which are very general can suffer from low power. This method exploits any existing relationship between illness severity and treatment effect, and identifies the "sweet spot", the contiguous range of illness severity where the estimated treatment benefit is maximized. We further compute a bias-corrected estimate of the conditional average treatment effect (CATE) in the sweet spot, and a p-value. More information here: \url{https://arxiv.org/abs/2011.10157}.
#' @param treated A binary vector with one entry for every individual in the trial. The value 1 indicates that the individual was treated,
#'  and the value 0 indicates that they were a control.
#' @param covariates A numeric matrix containing rows of covariates for every individual in the trial (in the same order as in `treated`).
#' @param outcome The response we use to compute the treatment effect. In the binary case, this may be 1 if the patient lived, and 0 else.
#' @param family A string indicating the response type. Options are "binomial" (for logistic regression), "possion" (for poisson regression) or "gaussian" (for linear regression).
#' @param regularized Boolean indicating whether to use regularization in the risk score model. If TRUE, we use the optimal model (lambda.min) returned by cv.glmnet.
#' @param control_treated_ratio The (positive integer) number of controls for each treated individual.
#' @param risk_score_nfolds The (positive integer) number of folds we use in pre-validation for the risk score. The default is 10 folds.
#' @param ntrials_significance The number of bootstraps to run when computing the p-value. Default is 1000.
#' @param ntrials_bias The number of bootstraps to run when debiasing the estimate of the treatment effect. Default is 1000.
#' @param min_size_fraction The minimum sweet spot width to consider. (Can not contain fewer than four matched sets.)
#' @param max_size_fraction The maximum sweet spot width to consider. (Can not be larger than 1 - the full size of the data.)
#' @return A list of results, containing: \itemize{
#'         \item{"matches", the matrix of matched treated and control patients, their mean risk score and treatment effect estimate}
#'         \item{"risk_score_model", the fitted glmnet object that models the risk score}
#'         \item{"risk_scores", the vector of prevalidated risk scores for each patient}
#'         \item{"model", the sweet spot model, a list containing:\itemize{
#'             \item{the start and end index of the sweet spot}
#'             \item{the bootstrapped p-value}
#'             \item{the mean inside and outside the sweet spot, before and after debiasing}
#'             \item{the distribution around the start and end indices, from the debiasing bootstrap}
#'             \item{the maximum statistic used to compute the p-value}
#'             }}
#' }
#' @include create_match_sets.R
#' @include risk_scores.R
#' @include find_sweetspot.R
#' @examples
#' # Example data with a sweet spot
#' # We generate data with a sweetspot in the middle 20% of risk scores.
#' # Outside the sweet spot, the treatment effect is 5%.
#' # Inside the sweet spot, the treatment effect is 45%.
#' set.seed(1234)
#' n <- 500; p <- 10;
#' treated    <- sample(c(0,1), n, replace=TRUE)
#' covariates <- matrix(rnorm(n * p), nrow=n, ncol=p)
#' beta       <- rnorm(p)
#' outcome.prob <- 1/(1+exp(-(covariates %*% beta)))
#' in.sweet.spot <- !is.na(cut(outcome.prob, c(.4, .6)))
#' outcome.prob[treated==1] <- outcome.prob[treated==1] + .05
#' outcome.prob[treated==1 & in.sweet.spot] <- outcome.prob[treated==1 & in.sweet.spot] + .4
#' outcome.prob <- pmin(outcome.prob, 1)
#' outcome <- rbinom(n, 1, prob=outcome.prob)
#'
#' # We limit the number of trials run only to illustrate how to use this function.
#' # In practice, we recommend more bootstrap trials.
#' result <- sweetspot(treated, covariates, outcome,
#'                     "binomial", ntrials_bias=250, ntrials_significance=250)
#' plot_sweetspot(result, title="Sweet spot on simulated data")
#'
#'
#' # Example data without a sweet spot.
#' # The overall treatment effect is 5%.
#' set.seed(1234)
#' n <- 500; p <- 10;
#' treated    <- sample(c(0,1), n, replace=TRUE)
#' covariates <- matrix(rnorm(n * p), nrow=n, ncol=p)
#' beta       <- rnorm(p)
#' outcome.probability <- 1/(1+exp(-(covariates %*% beta)))
#' outcome.probability[treated == 1] <- pmin(outcome.probability[treated == 1] + .05, 1)
#' outcome <- rbinom(n, 1, prob=outcome.probability)
#'
#' result <- sweetspot(treated, covariates, outcome,
#'                     "binomial", ntrials_bias=250, ntrials_significance=250)
#' plot_sweetspot(result, title="Sweet spot on simulated data")
#'
#' @export sweetspot

sweetspot <- function(treated, covariates, outcome, family,
                      regularized = F, control_treated_ratio = 1, risk_score_nfolds = 10,
                      ntrials_significance=1000, ntrials_bias=1000,
                      min_size_fraction = 1/20, max_size_fraction = 1){

  # Check types
  if(!((class(covariates)[1] == "matrix") & (typeof(covariates) == "double"))) {
    message("The covariates are not a numeric matrix."); stop()
  }

  if(!(family %in% c("binomial", "poisson", "gaussian"))){ message("family should be 'binomial', 'poisson' or 'gaussian'."); stop()}
  if(!is.numeric(c(ntrials_significance, ntrials_bias))){ message("Number of trials ('ntrials_significance', 'ntrials_bias') should be numeric"); stop() }
  if(floor(ntrials_significance) != ntrials_significance) { message("'ntrials_significance' should be an integer."); stop() }
  if(floor(ntrials_bias) != ntrials_bias) { message("'ntrials_bias' should be an integer."); stop() }

  # Compute the risk scores using prevalidation:
  risk_scores <- risk_scores(treated, covariates, outcome, family = family, regularized = regularized, nfolds  = risk_score_nfolds)
  risk_score_model = risk_scores$model
  risk_scores      = risk_scores$risk_scores

  # Match individuals using risk_scores:
  matches <- create_match_sets(treated, risk_scores, control_treated_ratio, outcome)

  # Find the sweet spot, estimate the p-value, and debias the CATE.
  model <- find_sweetspot(matches[, "treatment_effect"],
                          ntrials_significance = ntrials_significance,
                          ntrials_bias = ntrials_bias,
                          min_size_fraction = min_size_fraction,
                          max_size_fraction = max_size_fraction
  )

  return(list(
    matches = matches,
    risk_score_model = risk_score_model,
    risk_scores = risk_scores,
    model = model
  ))
}
