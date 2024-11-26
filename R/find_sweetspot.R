#' @title Find Optimal Sweet Spot for Treatment Effects
#' @description This function identifies the "sweet spot" in treatment effects by analyzing deviations from the global mean across potential window sizes. It computes statistics for all valid windows, evaluates statistical significance through resampling, and debiases the statistics using bootstrapping.
#' @param treatment_effect Numeric vector containing the observed treatment effects.
#' @param ntrials_significance Integer. The number of resampling trials for calculating the p-value. Default is 1000.
#' @param ntrials_bias Integer. The number of resampling trials for debiasing the computed statistics. Default is 1000.
#' @param min_size_fraction Numeric. The minimum size of a window as a fraction of the total length of `treatment_effect`. Default is 1/20.
#' @param max_size_fraction Numeric. The maximum size of a window as a fraction of the total length of `treatment_effect`. Default is 1/2.
#' @return A list with the following elements:
#' \describe{
#'   \item{start_index}{The starting index of the optimal window with the highest statistic.}
#'   \item{end_index}{The ending index of the optimal window with the highest statistic.}
#'   \item{statistic}{The maximum deviation of the treatment effect in the optimal window from the global mean.}
#'   \item{mean_inside}{The mean treatment effect for values inside the optimal window.}
#'   \item{mean_outside}{The mean treatment effect for values outside the optimal window.}
#'   \item{mean_inside_debiased}{The debiased mean treatment effect inside the optimal window, adjusted using bootstrap resampling.}
#'   \item{mean_outside_debiased}{The debiased mean treatment effect outside the optimal window, adjusted using bootstrap resampling.}
#'   \item{means}{A vector of bootstrapped means inside the optimal window, used for diagnostics.}
#'   \item{means_outside}{A vector of bootstrapped means outside the optimal window, used for diagnostics.}
#'   \item{start_indices}{The starting indices of windows sampled during debiasing.}
#'   \item{end_indices}{The ending indices of windows sampled during debiasing.}
#'   \item{bias_inside}{An estimate of the bias for the mean inside the optimal window.}
#'   \item{bias_outside}{An estimate of the bias for the mean outside the optimal window.}
#'   \item{p_value}{The p-value for the observed statistic, based on resampling.}
#' }
#' @author Danny Del Rosso, Maksim Helmann
#' @export


one_statistic <- function(k, benefits_cumsum, n, overall_mean, stat="mean") {
  # Compute the treatment effect for specified window size 'k' and substract the mean treatment effect
  # multiplied by the window size. This calculation evaluates how the treatment effect in a window
  # deviates from the overall mean effect.

  # Specifically:
  # - `benefits_cumsum[((k+1):n)]` gives the cumulative sums for all windows of size 'k' starting
  #   from index `k+1` to `n`.
  # - `benefits_cumsum[1:(n-k)]` gives the cumulative sums for all windows of size 'k' ending
  #   at index `n-k`.
  # - Subtracting these cumulative sums effectively calculates the sum of the elements in each
  #   window of size `k` (via the difference of cumulative sums).
  # - The term `k * overall_mean` adjusts the results by centering them around the expected
  #   treatment effect based on the global mean, which ensures that the values reflect
  #   deviations from this expectation.

  # The resulting `result` vector contains the deviation of the treatment effect in each window
  # of size `k` from the global mean treatment effect.
  result <- (c(benefits_cumsum[k],
               benefits_cumsum[((k+1):n)] - benefits_cumsum[1:(n-k)]) - k * overall_mean)

  # Find the index of the window with the maximum deviation from the global mean.
  # `which.max(result)` identifies the first occurrence of the maximum value in the `result` vector.
  best_idx <- which.max(result)

  # Return a vector containing:
  # - `best_idx`: The starting index of the window with the maximum result.
  # - `best_idx + k - 1`: The ending index of the window with the maximum result.
  # - `max(result, na.rm = TRUE)`: The maximum value of the deviations, ignoring any NA values.
  # - `k`: The size of the window being analyzed.
  return(
    c(best_idx, best_idx + k - 1, max(result, na.rm = TRUE), k)
  )
}


compute_all_statistics <- function(n, treatment_effect, benefits_cumsum, start, end, maxonly = F) {
  # Iterate over all possible window sizes from `start` to `end`.
  # For each window size `k`, compute statistics using `one_statistic()` and collect the results in a matrix.
  result <- do.call(rbind, lapply(start:end, function(k)
    one_statistic(k, benefits_cumsum, n, mean(treatment_effect))))

  # If only the maximum statistic value is requested, return the maximum deviation from the global mean
  # across all windows.
  if (maxonly) return(max(result[, 3]))

  # Identify the row in the result matrix corresponding to the window with the maximum deviation.
  # `which.max(result[, 3])` finds the index of the row where the third column (the maximum statistic value)
  # is the largest.
  best <- result[which.max(result[, 3]), ]

  # Return a result as a list:
  # - `start_index`: The starting index of the window with the maximum deviation.
  # - `end_index`: The ending index of the window with the maximum deviation.
  # - `mean_inside`: The mean treatment effect for the elements within the best window.
  # - `mean_outside`: The mean treatment effect for the elements outside the best window.
  # - `statistic`: The maximum deviation value from the global mean for the best window.
  return(
    list(start_index = best[1],
         end_index = best[2],
         mean_inside = sum(treatment_effect[best[1]:best[2]]) / best[4],
         mean_outside = sum(treatment_effect[-(best[1]:best[2])]) / (n - best[4]),
         statistic = best[3]
         )
    )
}


find_sweetspot <- function(treatment_effect, # Vector of observed treatment effects
                           ntrials_significance = 1000, # Number of trials for p-value computation
                           ntrials_bias = 1000, # Number of trials for debiasing the results
                           min_size_fraction = 1/20, # Minimum window size as a fraction of the total data
                           max_size_fraction = 1/2) { # Maximum window size as a fraction of the total data

  # Number of treatment effect values
  n <- length(treatment_effect)

  # Define the range of window sizes to search for the best window:
  # `search_start` ensures the minimum window size is at least 4 or the fraction of the dataset, whichever is larger.
  # `search_end` ensures the maximum window size does not exceed the specified fraction of the dataset.
  search_start <- max(4, floor(min_size_fraction * n))
  search_end <- floor(max_size_fraction * n)

  # Identify the window with the maximum deviation from the global mean:
  best_window <- compute_all_statistics(n, treatment_effect, cumsum(treatment_effect), search_start, search_end, maxonly = F)


  # Separate treatment effect values into those inside and outside the best window:
  values_inside <- treatment_effect[best_window$start_index:best_window$end_index]
  values_outside <- treatment_effect[-(best_window$start_index:best_window$end_index)]

  # Compute a p-value to test the significance of the observed best window:
  # For `ntrials_significance` iterations:
  # - Randomly shuffle the treatment effect values (sampling without replacement).
  # - Compute the maximum statistic for the shuffled data.
  # - Count the proportion of iterations where the shuffled statistic is greater than or equal
  #   to the observed best statistic.
  pvalue <- sum(
    sapply(1:ntrials_significance, function(i) {
      sampled <- sample(treatment_effect) # Random shuffle of the treatment effect
      as.numeric(compute_all_statistics(n, sampled, cumsum(sampled), search_start, search_end, maxonly = T) >= best_window$statistic)
    })
  )
  # Normalize the count to get the p-value
  pvalue <- pvalue / ntrials_significance

  # Perform debiasing using bootstrapping:
  # For `ntrials_bias` iterations:
  # - Create a synthetic dataset by sampling from values outside and inside the best window.
  # - Compute statistics for this synthetic dataset.
  debias_boot <- do.call(rbind, lapply(1:ntrials_bias, function(i) {
    sampled <- c(sample(values_outside, best_window$start_index - 1, replace = T), # Sample outside (before the window)
                 sample(values_inside, best_window$end_index - best_window$start_index + 1, replace = T), # Sample inside the window
                 sample(values_outside, max(0, n - best_window$end_index), replace = T)) # Sample outside (after the window)
    compute_all_statistics(n, sampled, cumsum(sampled), search_start, search_end, maxonly = F)
  }))

  return(
    list(
      start_index = best_window$start_index, # Start index of the best window
      end_index = best_window$end_index, # End index of the best window
      statistic = best_window$statistic, # Maximum deviation observed
      mean_inside = best_window$mean_inside, # Mean treatment effect inside the best window
      mean_outside = best_window$mean_outside, # Mean treatment effect outside the best window
      # Debiased means of treatment effect
      mean_inside_debiased = 2 * best_window$mean_inside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_inside"])),
      mean_outside_debiased = 2 * best_window$mean_outside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_outside"])),

      means = unlist(debias_boot[, "mean_inside"]), # Means inside the bootstrapped windows
      means_outside = unlist(debias_boot[, "mean_outside"]), # Means outside the bootstrapped windows
      start_indices = unlist(debias_boot[, "start_index"]), # Start indices of bootstrapped windows
      end_indices = unlist(debias_boot[, "end_index"]), # End indices of bootstrapped windows

      bias_inside = best_window$mean_inside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_inside"])), # Bias inside
      bias_outside = best_window$mean_outside - 1/ntrials_bias * sum(unlist(debias_boot[, "mean_outside"])), # Bias outside

      p_value = pvalue       # P-value from significance test

    )
  )
}
