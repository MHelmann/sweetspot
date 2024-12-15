#' Summarize the results of a Sweetspot analysis
#' @description Provide summary statistics for an object of class \code{sweetspot_object}.
#'
#' @param object An object of class \code{sweetspot_object}.
#'
#' @return The 6 key summary statistics of any sweetspot analysis.
#'
#' @aliases summary.sweetspot_object
#'
#' @usage \S3method{summary}{sweetspot_object}(object)
#'
#' @export

summary.sweetspot_object <- function(object) {
  return(list(
    dev_ratio = object$dev_ratio,
    mean_inside = object$model$mean_inside,
    mean_inside_debiased = object$model$mean_inside_debiased,
    mean_outside = object$model$mean_outside,
    mean_outside_debiased = object$model$mean_outside_debiased,
    p = object$model$p_value
  )
  )
}

