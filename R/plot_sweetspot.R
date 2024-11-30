# Extract the model and matches from the result
plot_sweetspot <- function(model, title){
  model   <- result$model
  matches <- result$matches
  scaled_risk <- matches[, "scaled_effect"]
  treatment_effect <- matches[, "treatment_effect"]
  n <- nrow(matches)

  # Fit a smoothing spline to the treatment effect estimates over the mean risk scores
  smoothed <- smooth.spline(matches[, "scaled_effect"], matches[, "treatment_effect"])


  # Define key points along the x-axis (risk scores)
  minimum          <- min(scaled_risk)
  maximum          <- max(scaled_risk)
  start_sweetspot  <- scaled_risk[model$start_index]
  end_sweetspot    <- scaled_risk[model$end_index]
  mean_outside_debiased <- model$mean_outside_debiased
  mean_inside_debiased <- model$mean_inside_debiased
  mean_inside <- model$mean_inside
  mean_outside <- model$mean_outside

  base <- 0

  # Define colors (customize as needed)
  colors <- c(
    "original"                  = "grey",
    "debiased"                  = "aliceblue",
    "Smoothed Treatment Effect" = "black"
  )
  library("ggplot2")
  # Initialize the ggplot object
  p <- ggplot()

  #Left Rectangle (biased)
  p <- p + geom_ribbon(
    aes(
      x = c(minimum, start_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_outside, mean_outside),
      fill = "original"
    ),
    alpha = 0.8
  )

  #Right Rectangle (biased)
  p <- p + geom_ribbon(
    aes(
      x = c(end_sweetspot, maximum),
      ymin = rep(base, 2),
      ymax = c(mean_outside, mean_outside),
      fill = "original"
    ),
    alpha = 0.8
  )

  #Middle Rectangle (biased)
  p <- p + geom_ribbon(
    aes(
      x = c(start_sweetspot, end_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_inside, mean_inside),
      fill = "original"
    ),
    alpha = 0.8
  )

  #Left Rectangle (unbiased)
  p <- p + geom_ribbon(
    aes(
      x = c(minimum, start_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_outside_debiased, mean_outside_debiased),
      fill = "debiased"
    ),
    alpha = 0.8  )

  #Right Rectangle (unbiased)
  p <- p + geom_ribbon(
    aes(
      x = c(end_sweetspot, maximum),
      ymin = rep(base, 2),
      ymax = c(mean_outside_debiased, mean_outside_debiased),
      fill = "debiased"
    ),
    alpha = 0.8
  )

  #Middle Rectangle (debiased)
  p <- p + geom_ribbon(
    aes(
      x = c(start_sweetspot, end_sweetspot),
      ymin = rep(base, 2),
      ymax = c(mean_inside_debiased, mean_inside_debiased),
      fill = "debiased"
    ),
    alpha = 0.8  )

  # Add the smoothed treatment effect line
  p <- p + geom_line(
    aes(
      x = smoothed$x,
      y = smoothed$y,
      color = "Smoothed Treatment Effect"
    ),
    size = 0.6
  )

  # Set the limits of the x-axis
  p <- p + xlim(minimum, maximum)

  # Customize the color and fill scales
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

  plot_title <- ""

  # Add labels and titles
  p <- p + labs(
    x = "Predilection Score",
    y = "Treatment Effect Estimate",
    title = title
  )

  p <- p + geom_label(aes(x = 0.9, y = 0.2, label = paste0("p = ", model$p_value)))

  p <- p +
    geom_point(
      aes(
        x = start_sweetspot,
        y = 0,
      ),
      color = "black", size = 2
    )

    p <- p +  geom_text(
      aes(
        x = start_sweetspot,
        y = 0 - 0.02,  # Slightly nudge the label above the point
        label = round(start_sweetspot, 2)  # Format x-value (rounded to 2 decimals)
      ),
      color = "black", size = 3
    )
      p <- p + geom_point(
        aes(
          x = end_sweetspot,
          y = 0,
        ),
        color = "black", size = 2
      )
    p <- p + geom_text(
      aes(
        x = end_sweetspot,
        y = 0 -  0.02,  # Slightly nudge the label above the point
        label = round(end_sweetspot, 2)  # Format x-value (rounded to 2 decimals)
      ),
      color = "black", size = 3)


    p <- p + theme_minimal(base_size = 12)




  return(p)
}

