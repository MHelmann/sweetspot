#' @title Plot Sweet Spot for Treatment Effects
#' @description This function visualizes treatment effect estimates using a smoothed spline, highlighting regions inside and outside the sweet spot with both original and debiased treatment effects.
#' @param result The `result` object, containing
#'        indices for the sweet spot and mean treatment effect estimates.
#' @param title A character string specifying the title of the plot.
#' @param hypothesis A character string representing a user-specified formulation of the null hypothesis for
#'        the statistical test, taking context into account."
#' @details The plot's x-axis is labeled based on the family of the model (binomial, poisson, or other). Additionally, it annotates the start and end of the sweet spot region.
#' @return A `ggplot` object representing the sweet spot visualization.
#' @examples
#' \dontrun{
#'   # Assuming `result` contains model output and match data
#'   plot <- plot_sweetspot(result$model, "Sweet Spot Analysis",
#'   "No sweet spot related to illness severity")
#'   print(plot)
#' }
#' @import ggplot2
#' @author Barrett Buhler, Danny Del Rosso, Erin Craig, Maksim Helmann
#' @export
plot_sweetspot <- function(result, title, hypothesis = "No sweet spot related to illness severity"){

  # Extract model, matches and family from the `result` object
  model <- result$model
  matches <- result$matches
  family <- result$family

  # Retrieve variables for plotting
  scaled_risk <- matches[, "scaled_effect"]
  treatment_effect <- matches[, "treatment_effect"]
  n <- nrow(matches)  # Number of matches

  # Clean the hypothesis string by removing trailing whitespace
  hypothesis <- trimws(hypothesis, which = "right")

  # Fit a smoothing spline to the treatment effect data
  smoothed <- smooth.spline(matches[, "scaled_effect"], matches[, "treatment_effect"], lambda = 0.2)

  # Define the range and key points of the x-axis (risk scores)
  minimum <- min(scaled_risk)
  maximum <- max(scaled_risk)
  start_sweetspot <- scaled_risk[model$start_index]
  end_sweetspot <- scaled_risk[model$end_index]

  # Mean treatment effect estimates for inside and outside sweet spots
  mean_outside_debiased <- model$mean_outside_debiased
  mean_inside_debiased <- model$mean_inside_debiased
  mean_inside <- model$mean_inside
  mean_outside <- model$mean_outside
  base <- 0  # Base y-axis value

  # Define colors for different elements
  colors <- c(
    "original" = "red",
    "debiased" = "lightblue",
    "Smoothed Treatment Effect" = "black"
  )

  # Initialize the ggplot object
  p <- ggplot()

  # Add rectangles for the "original" (biased) treatment effect estimates
  # Left region
  p <- p + geom_ribbon(
    aes(
      x = c(minimum, start_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_outside, mean_outside),
      fill = "original"
    ),
    alpha = 0.8
  )

  # Right region
  p <- p + geom_ribbon(
    aes(
      x = c(end_sweetspot, maximum),
      ymin = rep(base, 2),
      ymax = c(mean_outside, mean_outside),
      fill = "original"
    ),
    alpha = 0.8
  )

  # Middle region
  p <- p + geom_ribbon(
    aes(
      x = c(start_sweetspot, end_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_inside, mean_inside),
      fill = "original"
    ),
    alpha = 0.8
  )

  # Add rectangles for the "debiased" treatment effect estimates
  # Left region
  p <- p + geom_ribbon(
    aes(
      x = c(minimum, start_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_outside_debiased, mean_outside_debiased),
      fill = "debiased"
    ),
    alpha = 0.8
  )

  # Right region
  p <- p + geom_ribbon(
    aes(
      x = c(end_sweetspot, maximum),
      ymin = rep(base, 2),
      ymax = c(mean_outside_debiased, mean_outside_debiased),
      fill = "debiased"
    ),
    alpha = 0.8
  )

  # Middle region
  p <- p + geom_ribbon(
    aes(
      x = c(start_sweetspot, end_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_inside_debiased, mean_inside_debiased),
      fill = "debiased"
      ),
    alpha = 0.8
  )
  # Add the smoothed treatment effect line
  p <- p + geom_line(
    aes(
      x = smoothed$x,
      y = smoothed$y,
      color = "Smoothed Treatment Effect"
    ),
    linewidth = 0.6
  )

  # Set the x-axis limits
  p <- p + xlim(minimum, maximum)

  # Customize the legend for colors and fills
  p <- p + scale_color_manual(
    values = colors,
    breaks = names(colors),
    guide = guide_legend(title = NULL)
  )

  p <- p + scale_fill_manual(
    values = colors,
    breaks = c("original", "debiased"),
    guide = guide_legend(title = NULL)
  )

  # Add labels to the axes based on the family type
  if (family == "binomial") {
    p <- p + labs(
      x = "Probability",
      y = "Treatment Effect Estimate"
    )
  } else if (family == "poisson") {
    p <- p + labs(
      x = "Counts",
      y = "Treatment Effect Estimate"
    )
  } else {
    p <- p + labs(
      x = "Predilection score",
      y = "Treatment Effect Estimate"
    )
  }

  # Add the title and subtitle, including hypothesis and p-value
  p <- p + labs(
    title = title,
    subtitle = bquote(H[0]: .(hypothesis) * " (p = " * .(model$p_value) * ")")
  )

  # Add markers and labels for the sweet spot boundaries
  p <- p +
    geom_point(
      aes(
        x = start_sweetspot,
        y = 0
      ),
      color = "black", size = 2
    )
  p <- p + geom_text(
    aes(
      x = start_sweetspot,
      y = 0 - 0.02,  # Slightly nudge the label below the point
      label = round(start_sweetspot, 2)  # Round to 2 decimals
    ),
    color = "black", size = 3
  )

  p <- p + geom_point(
    aes(
      x = end_sweetspot,
      y = 0
    ),
    color = "black", size = 2
  )
  p <- p + geom_text(
    aes(
      x = end_sweetspot,
      y = 0 - 0.02,  # Slightly nudge the label below the point
      label = round(end_sweetspot, 2)  # Round to 2 decimals
    ),
    color = "black", size = 3
  )

  # Apply a minimal theme for aesthetics
  p <- p + theme_minimal(base_size = 12)

  # Return the constructed plot
  return(p)
}

