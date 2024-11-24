#' @title Calculate risk scores from prognostic data
#' @description This function uses cross-validated elastic net regression to compute the predilection scores of patients from an inputted set of covariates
#' @param treated A binary vector indicating treatment status (1 for treated, 0 for control).
#' @param covariates A matrix with rows corresponding to individuals and colums to covariates to include in the mode;.
#' @param response A numeric vector of response values for the individuals.
#' @param family A string indicating the model family to be fit. Default is "binomial".
#' @param regularized later
#' @param nfolds later
#' @import glmnet
#' @author Danny Del Rosso, Maksim Helmann
#' @export

risk_scores <- function(treated, covariates, response, family = "binomial", regularized = F, nfolds = NULL){

  #Argument Checking
  if( (family == "binomial") && (length(unique(response)) > 2) ){
    message("The response type contains more than two unique values. Use `family = \"gaussian\" for continuous outcomes and `family = \"poisson\" for counts."); stop()}

  if( family == "poisson" && !all(response %% 1 == 0 & response >= 0) ) {
    message("The response type does not contain strictly non-negative integers.  Use `family = \"gaussian\" for continuous outcomes."); stop()}

  if(!is.null(nfolds)){
    if(nfolds > sum(treated == 0)) { message("Too many folds for prevalidating predilection scores. Make sure the number of folds is less than or equal to the number of controls."); stop() }
  }

  folds <- ifelse(is.null(nfolds), sum(treated == 0),  nfolds)
  #Folds for CV defaults to the number of observations (LOOCV)

  model <- cv.glmnet(
    x = covariates[treated == 0, ],
    #Values of the covariates for the untreated group

    y = response[treated == 0],
    #Responses of the treatment group

    family = family,
    #Default value: Binary data

    type.measure = "deviance",
    #Default for GLMnet: Equivalent to MSE for continuous data

    nfolds = folds,

    grouped = FALSE,
    #Computes error for each observation and summarizes for each fold,
    #instead of computing one statistic for each fold
    keep = TRUE
  )

  s <- ifelse(regularized == F, model$lambda.min, model$lambda.1se)
  #Sets lambda based on regularization input

  s_idx <- which(model$lambda == s)
  #Retrieves the index of the lambda used

  dev_ratio <- round(model$glmnet.fit$dev.ratio[s_idx], 3)
  #the dev_ratio is the glmnet.cv analogue to the McFadden Pseudo-R2.

  nonzero <- model$nzero[s_idx]
  #number of nonzero coefficients

  #Fit check

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

  risk_scores[treated == 0] <- model$fit.preval[s_idx]
  #Selects the fitted values at the selected value of lambda

  risk_scores[treated == 1] <- predict(model, s = s, newx = covariates[treated == 1, ])
  #Predicts the values of the treated group using the established model

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
