#' make_chart
#'
#' @description A fct function to create a bar graph
#'
#' @import ggplot2
#' @import plotly
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# labels for y-axis

fct_label <- function(ghg_type) {
  if (ghg_type == "co2")
    return("Carbon dioxide")
  if (ghg_type == "n2o")
    return("Carbon dioxide")
  if (ghg_type == "soil.carbon.co2")
    return("Soil carbon")
  if (ghg_type == "total.ghg.co2")
    return("Total GHG")
}

fct_plot <- function(data, ghg_type) {

  # require data
  req(data)


  # subset data based on chosen ghg type
  data <- data |>
    subset(ghg_type = ghg_type)


  # plot data
  plot <-
    ggplot(data, aes(x = factor(implementation),
                   y = mean,
                   fill = county,
                   text = paste("Practice Implementation: ", implementation,
                                "\nCounty: ", county,
                                "\nGHG Coefficient: ", mean)
                   )
         ) +
    scale_fill_viridis_d() +
    geom_bar(stat = "identity",
             position = position_dodge()
             ) +
    geom_errorbar(
      ymin = data$lower,
      ymax = data$upper,
      width = 0.05,
      stat = "identity",
      position = position_dodge(0.9)
    ) +
    labs(
      fill = "County",
      x = "Practice Implementation",
      y = paste(fct_label(ghg_type), "\n(MT CO2e/ac/yr)"),
      title = paste(fct_label(ghg_type))
    ) +
    coord_flip() +
    scale_x_discrete(
      labels = wrap(data$implementation, 25)
    ) +
    theme_classic()


    # plot with ggplotly
  ggplotly <-
    ggplotly(plot, tooltip = c("text")) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = list(
             "sendDataToCloud",
             "pan2d",
             "zoomIn2d",
             "zoomOut2d",
             "select2d",
             "lasso2d",
             "hoverClosestCartesian",
             "hoverCompareCartesian",
             "autoScale2d"
           ))

}
