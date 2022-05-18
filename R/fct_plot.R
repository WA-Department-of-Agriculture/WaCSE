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

# TODO:  fix scales so upper error bar is not cut off
#         don't show error bars if data is not there
#         reverse legend order to match the order of the bars

fct_plot <- function(data, ghg_type) {
  # require data
  req(data)

  # subset data based on chosen ghg type
  data <- data |>
    subset(ghg_type = ghg_type)


  # plot data
  plot <-
    ggplot(data,
           aes(
             x = factor(implementation),
             y = mean,
             fill = forcats::fct_rev(county),
             text = paste(
               "Practice Implementation: ",
               implementation,
               "\nCounty: ",
               county,
               "\nGHG Coefficient: ",
               mean
             )
           )) +
    scale_fill_viridis_d(breaks = rev, direction = -1) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_errorbar(
      ymin = data$lower,
      ymax = data$upper,
      width = 0.03,
      size = 0.05,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      fill = "County",
      x = NULL,
      y = paste(fct_label(ghg_type), "\n(MT CO2e/ac/yr)")
    ) +
    coord_flip(expand = TRUE) +
    scale_x_discrete(
      labels = wrap(data$implementation, 100),
      guide = guide_axis(n.dodge = 2)
    ) +
    theme_classic()



  # plot with ggplotly
  ggplotly <-
    ggplotly(plot, tooltip = c("text")) %>%
    config(
      displaylogo = FALSE,
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
      )
    )

}
