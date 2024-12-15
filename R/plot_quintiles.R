#' @title Plot Treatment Effect at Different Quintiles
#' @description  This function visualizes treatment effect estimates by splitting the patients into quintiles of increasing disease severity and computing the Average Treatment Effect (ATE) in each.
#' @param result The output of the "sweetspot" wrapper function.
#' @param title A character string specifying the title of the plot.
#' @param pvalue An inputted p-value for a user-performed significance test. Defaults to the p-value from sweetspot.
#' @return A `ggplot` object representing the sweet spot visualization.
#' @examples
#' \dontrun{
#'   # Assuming `result` contains model output and match data
#'   plot <- plot_sweetspot(result, "Quintile_Plot",
#'   lines = T)
#'   print(plot)
#' }
#' @import ggplot2
#' @include split_quintiles.R
#' @author Danny Del Rosso, Maksim Helmann
#' @export

plot_quintiles <- function(result, title, pvalue = NULL){


  # Assign p-value based on input
  if(!is.null(pvalue)){
    plot_pvalue <- pvalue
  } else{
    plot_pvalue <- result$model$p_value
  }

  # Split the data into quintiles
  quintiles <- split_quintiles(result)

  # Calculate the Average Treatment Effect (ATE) in each quintile
  qmeans <- sapply(quintiles, mean)

  # Store data in a data frame for ease of plotting
  plotdata <- data.frame(
    quintile = names(qmeans),
    mean = as.numeric(qmeans)
  )

  # Initialize bar chart
  p <- ggplot(plotdata, aes(x = quintile, y = mean, fill = quintile))

  p <- p + geom_col()

  # Colour unimportant quintiles a lighter colour
  p <- p + scale_fill_manual(values = c(q1 = "lightblue", q2 = "#ADD8E664",
                                        q3 = "lightblue", q4 = "#ADD8E664",
                                        q5 = "lightblue")) +
    labs(title = title, x = "Quintile", y= "Average Treatment Effect")


  # Add lines for the computed sweetspot
  if(lines == T){
    p <- p +  geom_hline(yintercept = inside_mean, linetype = "dotted", color = "black", linewidth = 1) +
      geom_hline(yintercept = outside_mean, linetype = "dotted", color = "black", linewidth = 1)
  }

  #Alter the plot appearance
  p <- p + theme_classic() + theme(legend.position = "none") + theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans"),
    axis.title.y = element_text(size = 14, family = "sans"),
    axis.text.x = element_text(size = 12, family = "sans"),
    axis.text.y = element_text(size = 12,  family = "sans")
  )

  #Add the p-value
  p <- p + geom_text(
    aes(x = 5, y = max(qmeans), label = paste("p =", plot_pvalue)),
    size = 5, # Font size
    color = "black" # Text color
  )

  return(p)
}

