#' @title Perform a Sweet Spot Analysis on Clinical Trial Data
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
#' @include compute_avg_treat_effect.R
#' @include risk_scores.R
#' @include find_sweetspot.R
#' @examples
#' # Example scenario with a sweet spot
#' # Artificial sweetspot is generated with a sweetspot in the middle 20% of risk scores.
#' # Outside the sweet spot, the treatment effect is 5%.
#' # Inside the sweet spot, the treatment effect is 35%.
#' set.seed(125)
#' # Parameters
#' n <- 1000 # Number of individuals
#' p <- 5 # Number of covariates
#' sweet_spot_range <- c(0.4, 0.6)  # Sweet spot range for probabilities
#' # Generate a balanced treatment assignment
#' treated <- rep(c(0, 1), each = n / 2)
#' # Simulate covariates
#' covariates <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' # Generate coefficients for covariates
#' beta <- rnorm(p)
#' # Compute baseline probabilities using logistic regression
#' baseline_probs <- 1 / (1 + exp(-covariates %*% beta))
#' baseline_probs <- pmin(pmax(baseline_probs, 0), 1)  # Keep probabilities in [0, 1]
#' # Identify individuals in the sweet spot
#' in_sweet_spot <- baseline_probs > sweet_spot_range[1] & baseline_probs < sweet_spot_range[2]
#' # Adjust probabilities for treated individuals
#' treatment_effect <- 0.05 # Base treatment effect
#' sweet_spot_effect <- 0.3 # Additional effect in sweet spot
#' outcome_probs <- baseline_probs
#' outcome_probs[treated == 1] <- outcome_probs[treated == 1] + treatment_effect
#' outcome_probs[treated == 1 & in_sweet_spot] <- outcome_probs[treated == 1 & in_sweet_spot] + sweet_spot_effect
#' outcome_probs <- pmin(outcome_probs, 1)  # Cap probabilities at 1
#' # Simulate outcomes
#' outcome <- rbinom(n, 1, prob = outcome_probs)
#' # Next we use the sweetspot wrapper and plot_sweetspot function to display the results of the sweet spot analysis
#' # Perform sweet spot analysis
#' result <- sweetspot(treated = treated, covariates = covariates, outcome = outcome, family = "binomial", regularized = FALSE, control_treated_ratio = 1)
#'  # Plot the sweet spot
#' plot_sweetspot(result, hypothesis="Sweetspot does not exist", title = "Sweet Spot Analysis on Simulated Data")
#'
#'
#' # Example scenario without a sweet spot
#' # The overall treatment effect is 5%.
#' set.seed(1234)
#' #' n <- 1000 # Number of individuals
#' p <- 5 # Number of covariates
#' # Generate a balanced treatment assignment
#' treated <- rep(c(0, 1), each = n / 2)
#' # Simulate covariates
#' covariates <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' # Generate coefficients for covariates
#' beta <- rnorm(p)
#' covariates <- matrix(rnorm(n * p), nrow=n, ncol=p)
#' beta       <- rnorm(p)
#' # Compute baseline probabilities using logistic regression
#' baseline_probs <- 1 / (1 + exp(-covariates %*% beta))
#' baseline_probs <- pmin(pmax(baseline_probs, 0), 1)  # Keep probabilities in [0, 1]
#' # Adjust probabilities for treated individuals
#' treatment_effect <- 0.05 # Base treatment effect
#' outcome_probs <- baseline_probs
#' outcome_probs[treated == 1] <- outcome_probs[treated == 1] + treatment_effect
#' outcome_probs <- pmin(outcome_probs, 1)  # Cap probabilities at 1
#' # Simulate outcomes
#' outcome <- rbinom(n, 1, prob = outcome_probs)
#' # Next we use the sweetspot wrapper and plot_sweetspot function to display the results of the sweet spot analysis
#' # Perform sweet spot analysis
#' result <- sweetspot(treated = treated, covariates = covariates, outcome = outcome, family = "binomial", regularized = FALSE, control_treated_ratio = 1)
#'  # Plot the sweet spot
#' plot_sweetspot(result, hypothesis="Sweetspot does not exist", title = "Sweet Spot Analysis on Simulated Data")
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

  # Cast family string
  family <- tolower(family)

  if(!(family %in% c("binomial", "poisson", "gaussian"))){ message("family should be 'binomial', 'poisson' or 'gaussian'."); stop()}
  if(!is.numeric(c(ntrials_significance, ntrials_bias))){ message("Number of trials ('ntrials_significance', 'ntrials_bias') should be numeric"); stop() }
  if(floor(ntrials_significance) != ntrials_significance) { message("'ntrials_significance' should be an integer."); stop() }
  if(floor(ntrials_bias) != ntrials_bias) { message("'ntrials_bias' should be an integer."); stop() }

  # Compute the risk scores using prevalidation:
  risk_scores <- risk_scores(treated, covariates, outcome, family = family, regularized = regularized, nfolds  = risk_score_nfolds)
  risk_score_model <- risk_scores$model
  scaled_effect <- risk_scores$pred_response
  dev_ratio <- risk_scores$dev_ratio
  risk_scores      <- risk_scores$risk_scores
  # Match individuals using risk_scores:
  matches <- create_match_sets(treated, risk_scores, control_treated_ratio)
  matched_sets <- matches$matched_sets
  patients_df <- matches$patients_df

  matched_set_summary <- compute_avg_treat_effect(matched_sets, patients_df, risk_scores, scaled_effect, control_treated_ratio, outcome)

  # Find the sweet spot, estimate the p-value, and debias the CATE.
  model <- find_sweetspot(matched_set_summary[, "treatment_effect"],
                          ntrials_significance = ntrials_significance,
                          ntrials_bias = ntrials_bias,
                          min_size_fraction = min_size_fraction,
                          max_size_fraction = max_size_fraction
  )

  result <- list(
    matches = matched_set_summary,
    risk_score_model = risk_score_model,
    scaled_effect = scaled_effect,
    risk_scores = risk_scores,
    dev_ratio = dev_ratio, #HERE
    model = model,
    family = family
  )

  class(result) <- "sweetspot_object"
  return(result)
}


