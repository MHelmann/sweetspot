#' @title Compute Average Treatment Effect
#' @description Compute the mean risk score and treatment effect for each matched set.
#' @param matched_sets A vector indicating the matched group assignments for each individual. The names of the vector correspond to individual IDs.
#' @param patients_df A data frame containing treatment status, predilection score and patient id.
#' @param risk_scores A numeric vector of risk scores for all individuals.
#' @param scaled_effect A numeric vector of back-transformed risk scores depending on the family (e.g., probability for "binomial").
#' @param control_treated_ratio An integer specifying the ratio of controls to treated individuals for matching.
#' @param response A numeric vector of response values for the individuals.
#' @return A matrix where each row corresponds to a matched set. The columns include:
#' - IDs of controls and treated individuals in each matched set.
#' - Risk scores for controls and treated individuals.
#' - Mean risk score of the matched set.
#' - Treatment effect for the matched set.
#' - Mean scaled effect for the matched set.
#' The rows are ordered by the mean risk score of the matched sets.
#' @details This function processes matched groups to compute the treatment effect and risk scores for each matched set. It identifies the controls and treated individuals in each group and computes the treatment effect as the difference in response values between treated and control individuals. The results are sorted by the mean risk score of the matched sets for easier interpretation.
#' @author Danny Del Rosso, Maksim Helmann
#' @export

compute_avg_treat_effect <- function(matched_sets, patients_df, risk_scores, scaled_effect, control_treated_ratio, response){
  # Compute information for each match (mean risk score, treatment effect)
  get_group_info <- function(group){

    # Extract indices of the group members
    individual_idx <- as.numeric(names(matched_sets[matched_sets == group]))
    # Index of controls (individuals in the group who are not treated)
    ctrls <- individual_idx[individual_idx %in% patients_df[patients_df$trt == 0, "idx"]]
    # Index of treated individuals (individuals in the group who are treated)
    trtds <- individual_idx[individual_idx %in% patients_df[patients_df$trt == 1, "idx"]]

    # Compute treatment effect
    treatment_effect <- mean(response[trtds] - response[ctrls])

    c(ctrls, trtds,                                       # id of each control/treated
      risk_scores[ctrls],                                 # risk scores for controls
      risk_scores[trtds],                                 # risk scores for treated
      mean(c(risk_scores[ctrls], risk_scores[trtds])),    # mean score for the group
      treatment_effect,                                   # treatment effect
      mean(c(scaled_effect[ctrls], scaled_effect[trtds])) # mean scaled effect for the group
    )
  }

  # Extract matching information for all unique matched groups:
  # - 'unique(matched_sets)' gets all unique match identifiers from 'matched_sets'.
  # - 'sapply(unique(matched_sets), get_group_info)' applies the 'get_group_info' function to each unique match.
  # - 't()' transposes the result to arrange it in a tabular format.
  all_matches <- t(sapply(unique(matched_sets), get_group_info))

  # Create column names for the matched groups:
  # - If the control-to-treated ratio is 1, use "control" as the column name.
  # - Otherwise, create separate names for each control (e.g., "control_1", "control_2", etc.).
  ctrl_names <- if(control_treated_ratio == 1) {"control"} else {paste0("control_", 1:control_treated_ratio)}

  colnames(all_matches) <- c(ctrl_names, "treated", paste0(ctrl_names, "_score"), "treated_score", "mean_score", "treatment_effect", "scaled_effect")
  # return the matches in order of increasing risk score within the group
  return(
    all_matches[order(all_matches[, "mean_score"]), ]
  )
}
