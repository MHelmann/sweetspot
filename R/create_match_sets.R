#' @title Create Matched Sets Using Pair Matching
#' @description This function creates matched sets of treated and control individuals using pair matching based on a risk score.
#' @param treated A binary vector indicating treatment status (1 for treated, 0 for control).
#' @param risk_scores A numeric vector of risk scores for all individuals.
#' @param control_treated_ratio An integer specifying the ratio of controls to treated individuals for matching.
#' @return A list containing the following elements:
#'         \itemize{
#'         \item{"matched_sets"}: The result of the pair matching process, containing matched sets of treated and control individuals.
#'         \item{"patients_df"}: A data frame containing the treatment status, risk scores, and indices of all individuals.
#'         }
#' @importFrom optmatch pairmatch
#' @details This function uses the `pairmatch()` function from the `optmatch` package to match treated and control individuals based on their risk scores. It supports specifying a control-to-treated ratio and removes any unmatched individuals from the results.
#' @author Danny Del Rosso, Erin Craig, Maksim Helmann
#' @export


create_match_sets <- function(treated, risk_scores, control_treated_ratio){
  # Check if 'treated' is binary
  if (!all(treated %in% c(0, 1))) {
    stop("Error: The 'treated' vector must be binary (containing only 0 and 1).")
  }
  # Check if control_treated_ratio is an integer
  if (!is.numeric(control_treated_ratio) || control_treated_ratio != as.integer(control_treated_ratio) || control_treated_ratio <= 0) {
    stop("Error: 'control_treated_ratio' must be a positive integer.")
  }

  # Create a data frame containing treatment status, risk scores, and indices of individuals
  df <- data.frame(trt=treated, score=risk_scores, idx=1:length(treated))

  # Perform pair matching using the 'pairmatch()' function:
  # - Matches individuals based on their treatment status ('trt') and risk scores ('score').
  # - 'controls' specifies the ratio of controls to treated individuals for matching.^
  # - Unmatched individuals are removed automatically using the `remove_unmatchables` option.
  matched_sets <- pairmatch(trt ~ score, data=df, controls=control_treated_ratio, remove_unmatchables = TRUE)

  # Remove any NA values from the matching results to keep only successfully matched sets.
  if (any(is.na(matched_sets))) {
    print("Warning: A matching result was not found for some individuals that lacked counterpart within caliper distance. Removing unmatched individuals.")
    matched_sets <- matched_sets[!is.na(matched_sets)]  # Remove NA values
  }

  return(list(
    matched_sets = matched_sets,
    patients_df = df
  ))
}

