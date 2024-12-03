#' @title Create Matched Sets Using Pair Matching
#' @description This function creates matched sets of treated and control individuals using pair matching based on a risk score. It computes the mean risk score and treatment effect for each matched set.
#' @param treated A binary vector indicating treatment status (1 for treated, 0 for control).
#' @param risk_scores A numeric vector of risk scores for all individuals.
#' @param control_treated_ratio An integer specifying the ratio of controls to treated individuals for matching.
#' @param response A numeric vector of response values for the individuals.
#' @return A matrix where each row corresponds to a matched set. The columns include:
#' - IDs of controls and treated individuals in each matched set.
#' - Risk scores for controls and treated individuals.
#' - The mean risk score of the matched set.
#' - The treatment effect for the matched set.
#' The rows are ordered by the mean risk score of the matched sets.
#' @importFrom optmatch pairmatch
#' @author Danny Del Rosso, Maksim Helmann
#' @export

create_match_sets <- function(treated, risk_scores, scaled_effect, control_treated_ratio, response){
  # Check if 'treated' is binary
  if (!all(treated %in% c(0, 1))) {
    stop("Error: The 'treated' vector must be binary (containing only 0 and 1).")
  }
  # Check if control_treated_ratio is an integer
  if (!is.numeric(control_treated_ratio) || control_treated_ratio != as.integer(control_treated_ratio) || control_treated_ratio <= 0) {
  stop("Error: 'control_treated_ratio' must be a positive integer.")
  }


  df <- data.frame(trt=treated, score=risk_scores, idx=1:length(treated))

  # Perform pair matching using the 'pairmatch()' function:
  # - Matches individuals based on their treatment status ('trt') and risk scores ('score').
  # - 'controls' specifies the ratio of controls to treated individuals for matching.
  pm <- pairmatch(trt ~ score, data=df, controls=control_treated_ratio, remove_unmatchables = TRUE)
  # Remove any NA values from the matching results to keep only successfully matched pairs.
  if (any(is.na(pm))) {
  print("Warning: A matching result was not found for some individuals that lacked counterpart within caliper distance. Removing unmatched individuals.")
  pm <- pm[!is.na(pm)]  # Remove NA values
  }

  # Compute information for each match (mean risk score, treatment effect)
  get_group_info <- function(group){

    # Extract indices of the group members
    individual_idx <- as.numeric(names(pm[pm == group]))
    # Index of controls (individuals in the group who are not treated)
    ctrls <- individual_idx[individual_idx %in% df[df$trt == 0, "idx"]]
    # Index of treated individuals (individuals in the group who are treated)
    trtds <- individual_idx[individual_idx %in% df[df$trt == 1, "idx"]]

    # Compute treatment effect
    treatment_effect <- mean(response[trtds] - response[ctrls])

    c(ctrls, trtds,                                    # id of each control/treated
      risk_scores[ctrls],                              # risk scores for controls
      risk_scores[trtds],                              # risk scores for treated
      mean(c(risk_scores[ctrls], risk_scores[trtds])), # mean score for the group
      treatment_effect,                                # treatment effect
      mean(c(scaled_effect[ctrls], scaled_effect[trtds]))
    )
  }

  # Extract matching information for all unique matched groups:
  # - 'unique(pm)' gets all unique match identifiers from 'pm'.
  # - 'sapply(unique(pm), get_group_info)' applies the 'get_group_info' function to each unique match.
  # - 't()' transposes the result to arrange it in a tabular format.
  all_matches <- t(sapply(unique(pm), get_group_info))

  # Create column names for the matched groups:
  # - If the control-to-treated ratio is 1, use "control" as the column name.
  # - Otherwise, create separate names for each control (e.g., "control_1", "control_2", etc.).
  ctrl_names <- if(control_treated_ratio == 1) {"control"} else {paste0("control_", 1:control_treated_ratio)}

  colnames(all_matches) <- c(ctrl_names, "treated", paste0(ctrl_names, "_score"), "treated_score", "mean_score", "treatment_effect", "scaled_effect")
  # return the matches in order of increasing risk score
  all_matches[order(all_matches[, "mean_score"]), ]

}

