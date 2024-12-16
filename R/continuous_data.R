#' Example continuous data
#'
#' Data from a simulated study investigating the effects of a treatment, statomycin, on a continuous outcome, change in systolic blood pressure (SBP) over a 1-year period.
#' The data was simulated such that statomycin increases the expected SBP change over the period
#' by 4 mmHg for patients who were expected to change by less than 1 mmHg, and by 1 mm Hg for all other patients.
#' All continuous variables included were z-scored.
#' @docType data
#' @usage data(continuous_data)
#' @format A data frame with 1000 rows and 12 variables:
#' \describe{
#'   \item{SBP Change}{The change in the systolic blood pressure (in mmHg) of patients over the 1-year study duration.}
#'   \item{Statomycin (Y/N)}{`1` if the patient received statomycin, `0` otherwise.}
#'   \item{Age Z-score}{The patient's standardized age.}
#'   \item{Physical Activity Score}{The patient's standardized physical activity level, measured by questionnaire.}
#'   \item{Diabetes (Y/N)}{`1` if the patient has diabetes, `0` otherwise.}
#'   \item{Dietary Score}{The patient's standardized concordance with the Canada Food Guide, measured by questionnaire.}
#'   \item{Air Quality Index}{The standardized air quality of the patient's hometown.}
#'   \item{CAD (Y/N)}{`1` if the patient has coronary artery disease, `0` otherwise.}
#'   \item{BMI Z-score}{The patient's BMI, relative to age and sex.}
#'   \item{Stress Score}{The patient's day-to-day stress levels, measured by questionnaire.}
#'   \item{Sex (M/F)}{`1` if the patient is male, `0` otherwise.}
#'   \item{Happiness Score}{The patient's satisfaction with their life, measured by questionnaire.}
#' }
#'
"continuous_data"
