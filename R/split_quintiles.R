#' @title Separate Treatment Outcomes Into Quintiles
#' @description This function allows the user to split the patients into quintiles based on baseline risk and examine the outcomes in each quintile.
#' @param result The output of the "sweetspot" wrapper function.
#' @return A list of vectors at each quintile containing the treatment effects of the relevant patients.
#' @examples
#' \dontrun{
#'   # Assuming `result` contains model output and match data
#'   quintiles <- split_quintiles(result)
#'   print(quintiles)
#' }
#' @author Danny Del Rosso, Maksim Helmann
#' @export

split_quintiles <- function(result){

  # Obtaining a vector of the treatment effect in each match, ordered by predilection score
  ordered_match_effect <- result$matches[, "treatment_effect"]

  # Breaking the values into quintiles of predilection score, retaining the most data in the first, third, and fifth quintile
  n <- length(ordered_match_effect)
  size <- ceiling(n/ 5)
  rem <- ifelse(n %% 5 == 0, 0, 5 - n %% 5)

  # This ensures that the first, third, and fifth quintiles all have number of observations equal to "size"
  breaks <- c(0, size, 2*size - floor(rem/2), 3*size - floor(rem/2),  n - size, n)

  # Define the quintiles according to the prespecified breaks
  group_factor <- cut(seq_along(ordered_match_effect),
                      breaks = breaks,
                      include.lowest = T,
                      labels = c("q1", "q2", "q3", "q4", "q5"))

  quintiles <- split(ordered_match_effect, group_factor)

  return(quintiles)
}
