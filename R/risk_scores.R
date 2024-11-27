#' @title Calculate risk scores from prognostic data
#' @description This function uses cross-validated elastic net regression to compute the predilection scores of patients from an input set of covariates
#' @param treated A binary vector indicating treatment status (1 for treated, 0 for control).
#' @param covariates A matrix where rows correspond to individuals and columns to covariates
#' to be included in the model.
#' @param response A numeric vector of response values for the individuals.
#' @param family A string indicating the model family to be fit. Options include `"binomial"`,
#' `"poisson"`, or `"gaussian"`. Default is `"binomial"`.
#' @param regularized A boolean indicating whether to select a stronger, more regularized penalty
#' (e.g., `lambda.1se`) when computing the scores. Default is `FALSE`.
#' @param nfolds The number of folds for cross-validation. If `NULL`, leave-one-out
#' cross-validation (LOOCV) is used.
#' @return A list with the following 5 elements:
#' \describe{
#'   \item{model}{The `cv.glmnet` object.}
#'   \item{risk_scores}{A numeric vector containing the computed predilection scores.}
#'   \item{pred_response}{The risk scores transformed back to their original scale
#'   (e.g., probabilities instead of log-odds for the "binomial" family).}
#'   \item{dev_ratio}{The deviance ratio for assessing model fit:
#'   \eqn{R^2} for continuous outcomes, or McFadden's Pseudo-\eqn{R^2} for non-continuous outcomes.}
#'   \item{nonzero}{The number of nonzero coefficients in the final model.}
#' }
#' @import glmnet
#' @author Danny Del Rosso, Maksim Helmann
#' @export

risk_scores <- function(treated, covariates, response, family = "binomial", regularized = F, nfolds = NULL){

  # Argument Checking
  if( (family == "binomial") && (length(unique(response)) > 2) ){
    message("The response type contains more than two unique values. Use `family = \"gaussian\" for continuous outcomes and `family = \"poisson\" for counts."); stop()}

  if( family == "poisson" && !all(response %% 1 == 0 & response >= 0) ) {
    message("The response type does not contain strictly non-negative integers.  Use `family = \"gaussian\" for continuous outcomes."); stop()}

  if(!is.null(nfolds)){
    if(nfolds > sum(treated == 0)) { message("Too many folds for prevalidating predilection scores. Make sure the number of folds is less than or equal to the number of controls."); stop() }
  }

  # Folds for CV defaults to the number of observations (LOOCV)
  folds <- ifelse(is.null(nfolds), sum(treated == 0),  nfolds)

  model <- cv.glmnet(

    # Values of the covariates for the untreated group
    x = covariates[treated == 0, ],

    # Responses of the treatment group
    y = response[treated == 0],

    # Default is "binomial" for binary outcome
    family = family,

    # Default for GLMnet: Equivalent to MSE for continuous data
    type.measure = "deviance",

    # Folds for cross-validation
    nfolds = folds,

    # Compute error for each observation and summarize for each fold,
    # instead of computing one statistic for each fold
    grouped = FALSE,

    # Retains predicted values for each observation used to calculate loss
    keep = TRUE
  )

  # Setting lambda based on regularization input
  s <- ifelse(regularized == F, model$lambda.min, model$lambda.1se)

  # Retrieving the index of the lambda used
  s_idx <- which(model$lambda == s)

  # the dev_ratio is the glmnet.cv analogue to the McFadden Pseudo-R2. For continuous data, it is the R2-value
  dev_ratio <- round(model$glmnet.fit$dev.ratio[s_idx], 3)

  # number of nonzero coefficients
  nonzero <- model$nzero[s_idx]

  # Fit check
  if ((dev_ratio < 0.15 || ncol(covariates) - nonzero > 0.5 * ncol(covariates)) && family %in% c("binomial", "poisson")){
    warning(sprintf(
      "Model fit may be poor. Coefficients nonzero after fitting: %d (dev.ratio = %.3f)",
      nonzero, dev_ratio
    ))
  }

  if ((dev_ratio < 0.15 || ncol(covariates) - nonzero > 0.5 * ncol(covariates)) && family == "gaussian") {
    warning(sprintf(
      "Model fit may be poor. Coefficients nonzero after fitting: %d (dev.ratio = %.3f)",
      nonzero, dev_ratio
    ))
  }
  risk_scores <- rep(NA, length(response))

  # Selecting the fitted values at the selected value of lambda
  risk_scores[treated == 0] <- model$fit.preval[s_idx]

  # Predicting the values of the treated group using the established model
  risk_scores[treated == 1] <- stats::predict(model, s = s, newx = covariates[treated == 1, ])

  # Adjusting scale of response
  if (family == "binomial"){
    pred_response <- 1/(1 + exp(-(risk_scores))) #inverse logit
  } else if (family == "poisson"){
    pred_response <- exp(risk_scores)
  } else{
    pred_response <- risk_scores
  }

  return(list(model = model,
              risk_scores = risk_scores,
              pred_response = pred_response,
              dev_ratio = dev_ratio,
              nonzero = nonzero))
}
