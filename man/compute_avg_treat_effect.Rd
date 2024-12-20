% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_avg_treat_effect.R
\name{compute_avg_treat_effect}
\alias{compute_avg_treat_effect}
\title{Compute Average Treatment Effect}
\usage{
compute_avg_treat_effect(
  matched_sets,
  patients_df,
  risk_scores,
  scaled_effect,
  control_treated_ratio,
  response
)
}
\arguments{
\item{matched_sets}{A vector indicating the matched group assignments for each individual. The names of the vector correspond to individual IDs.}

\item{patients_df}{A data frame containing treatment status, predilection score and patient id.}

\item{risk_scores}{A numeric vector of risk scores for all individuals.}

\item{scaled_effect}{A numeric vector of back-transformed risk scores depending on the family (e.g., probability for "binomial").}

\item{control_treated_ratio}{An integer specifying the ratio of controls to treated individuals for matching.}

\item{response}{A numeric vector of response values for the individuals.}
}
\value{
A matrix where each row corresponds to a matched set. The columns include:
\itemize{
\item IDs of controls and treated individuals in each matched set.
\item Risk scores for controls and treated individuals.
\item Mean risk score of the matched set.
\item Treatment effect for the matched set.
\item Mean scaled effect for the matched set.
The rows are ordered by the mean risk score of the matched sets.
}
}
\description{
Compute the mean risk score and treatment effect for each matched set.
}
\details{
This function processes matched groups to compute the treatment effect and risk scores for each matched set. It identifies the controls and treated individuals in each group and computes the treatment effect as the difference in response values between treated and control individuals. The results are sorted by the mean risk score of the matched sets for easier interpretation.
}
\author{
Danny Del Rosso, Erin Craig, Maksim Helmann
}
