#' @title Find Optimal Sweet Spot for Treatment Effects
#' @description This function identifies the optimal "sweet spot" in treatment effects by computing statistics across possible window sizes, assessing significance through resampling, and debiasing the identified window's statistics.
#' @param treatment_effect A numeric vector containing the observed treatment effects.
#' @param ntrials_significance Integer. The number of resampling trials for calculating significance. Default is 100.
#' @param ntrials_bias Integer. The number of resampling trials for debiasing the computed statistics. Default is 100.
#' @param min_size_fraction Numeric. The minimum size of the window as a fraction of the total length. Default is 1/20.
#' @param max_size_fraction Numeric. The maximum size of the window as a fraction of the total length. Default is 1/2.
#' @return A list with elements:
#' \describe{
#'   \item{start_index}{The starting index of the optimal window.}
#'   \item{end_index}{The ending index of the optimal window.}
#'   \item{statistic}{The computed statistic for the optimal window.}
#'   \item{mean_inside}{Mean of values inside the optimal window.}
#'   \item{mean_outside}{Mean of values outside the optimal window.}
#'   \item{mean_inside_debiased}{Debiased mean of values inside the window.}
#'   \item{mean_outside_debiased}{Debiased mean of values outside the window.}
#'   \item{means}{Debiased means across bootstrap samples inside the window.}
#'   \item{means_outside}{Debiased means across bootstrap samples outside the window.}
#'   \item{start_indices}{Start indices of windows sampled during debiasing.}
#'   \item{end_indices}{End indices of windows sampled during debiasing.}
#'   \item{bias_inside}{Bias estimate for the mean inside the window.}
#'   \item{bias_outside}{Bias estimate for the mean outside the window.}
#'   \item{p_value}{The p-value for the observed treatment effect window.}
#' }
#' @importFrom foreach %dopar%
#' @author Danny Del Rosso, Maksim Helmann
#' @export


#library(doParallel)
#library(RcppRoll)
#library(foreach)
#library(parallel)

one_statistic <- function(k, benefits_cumsum, n, overall_mean, stat="mean"){
  # Compute total treatment_effect for specified window size 'k' and subtract the mean treatment effect multiplied by window size.
  # This procedure is done for all valid windows of size 'k' within the range of the cumulative sum.
  # Specifically:
  # - benefits_cumsum[((2 + k):n) - 1] gives the cumulative sums corresponding to the right edge of each window.
  # - benefits_cumsum[(2:(n - k)) - 1] gives the cumulative sums corresponding to the left edge of each window.
  # - Subtracting these cumulative sums isolates the sum of the treatment effect within each window of size 'k'.
  # - The term k * overall_mean adjusts for the expected treatment effect based on the overall mean,
  #   centering the results around how much each window deviates from this expectation.
  # The result is a measure of how much the treatment effect for each window deviates from the global mean.
  result  <- ((benefits_cumsum[((2+k):n)-1] - benefits_cumsum[(2:(n-k))-1]) - k*overall_mean)

  # Find the index of the window with the maximum result.
  # `which.max(result)` returns the index of the first occurrence of the maximum value in the `result` vector.
  best_idx <- which.max(result)

  # Return a vector containing:
  # - `best_idx+1`: The starting index of the window with the maximum result.
  # - `best_idx + k`: The ending index of the window with the maximum result.
  # - `max(result, na.rm = TRUE)`: The maximum value in the result vector, ignoring any NA values.
  # - `k`: The size of the window being analyzed.
  return(
    c(best_idx+1, best_idx+k, max(result, na.rm=T), k)
  )
}


compute_all_statistics <- function(n, treatment_effect, benefits_cumsum, start, end, maxonly=F){

  # Iterate over all possible window sizes from `start` to `end-1`.
  # For each window size `k`, compute statistics using `one_statistic()` and collect the results in a matrix.
  result <- do.call(rbind,  lapply(start:(end-1), function(k) one_statistic(k, benefits_cumsum, n, benefits_cumsum[n]/n))) # benefits_cumsum[n]/n = mean(treatment_effect)

  # If only the maximum statistic is requested, return the maximum deviation between treatment effect from the global mean.
  if(maxonly) return( max(result[, 3]) )

  # Identify the row in the matrix corresponding to the maximum statistic value.
  best   <- result[which.max(result[, 3]), ]

  # Construct and return a result as a list:
  # - `start_index`: Start index of the window with the maximum statistic.
  # - `end_index`: End index of the window with the maximum statistic.
  # - `mean_inside`: Mean treatment effect inside the best window.
  # - `mean_outside`: Mean treatment effect outside the best window.
  # - `statistic`: Maximum deviation between treatment effect for selected window (best[4]) from the global mean.
  return(
    list(start_index  = best[1],
         end_index    = best[2],
         mean_inside  = sum(treatment_effect[best[1]:best[2]])/best[4], #(benefits.cumsum[best[2]] - benefits.cumsum[best[1]-1])/best[4],
         mean_outside = sum(treatment_effect[-(best[1]:best[2])])/(n-best[4]), #(benefits.cumsum[n] - benefits.cumsum[best[2]] + benefits.cumsum[best[1]-1])/(n - best[4]),
         statistic    = best[3]
    )
  )
}



find_sweetspot <- function(treatment_effect, # treatment effect
                           ntrials_significance=100, ntrials_bias=100, # 1000, 1000
                           min_size_fraction = 1/20, max_size_fraction = 1/2){

  n                <- length(treatment_effect)
  search_start     <- max(4, floor(min_size_fraction*n))
  search_end       <- floor(max_size_fraction*n)

  # Compute the statistic for all possible windows on our observed data, and choose the largest:
  best_window      <- compute_all_statistics(n, treatment_effect, cumsum(treatment_effect), search_start, search_end, maxonly=F)

  # Debias:
  values_inside  <- treatment_effect[best_window$start_index:best_window$end_index]
  values_outside <- treatment_effect[-(best_window$start_index:best_window$end_index)]

  pvalue <- foreach(i=1:ntrials_significance, .combine = sum, .inorder=F, .multicombine=T, .maxcombine=ntrials_significance, .export = c("compute_all_statistics", "one_statistic")) %dopar% {
    sampled <- sample(treatment_effect)
    compute_all_statistics(n, sampled, cumsum(sampled), search_start, search_end, maxonly=T) >= best_window$statistic
  }
  pvalue <- pvalue/ntrials_significance

  debias_boot <- foreach(i=1:ntrials_bias, .combine = rbind, .inorder=F, .multicombine=T, .maxcombine=ntrials_bias, .export = c("compute_all_statistics", "one_statistic")) %dopar% {
    sampled <- c(sample(values_outside, best_window$start_index - 1, replace=T),
                 sample(values_inside,  best_window$end_index - best_window$start_index + 1, replace=T),
                 sample(values_outside, max(0, n - best_window$end_index), replace=T))
    compute_all_statistics(n, sampled, cumsum(sampled),  search_start, search_end, maxonly=F)
  }

  # Return:
  list(start_index  = best_window$start_index,
       end_index    = best_window$end_index,
       statistic    = best_window$statistic,
       mean_inside  = best_window$mean_inside,
       mean_outside = best_window$mean_outside,

       mean_inside_debiased  = 2 * best_window$mean_inside  - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_inside"])),
       mean_outside_debiased = 2 * best_window$mean_outside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_outside"])),

       means         = unname(unlist(debias_boot[, "mean_inside"])),
       means_outside = unname(unlist(debias_boot[, "mean_outside"])),
       start_indices = unname(unlist(debias_boot[, "start_index"])),
       end_indices   = unname(unlist(debias_boot[, "end_index"])),
       bias_inside   = best_window$mean_inside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_inside"])),
       bias_outside  = best_window$mean_outside- 1/ntrials_bias * sum(unlist(debias_boot[, "mean_outside"])),

       p_value      = pvalue
  )
}


