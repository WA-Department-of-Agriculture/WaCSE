#' make_chart
#'
#' @description A fct function to create a bar graph
#'
<<<<<<< HEAD
=======
#' @import ggplot2
#'
>>>>>>> 89a86cc (continued work on table and plot)
#' @return The return value, if any, from executing the function.
#'
#' @noRd

<<<<<<< HEAD
fct_plot <- function(data) {
  data |>
  dplyr::group_by(county) |>
  subset(ghg_type == "co2") |>
  echarts4r::e_charts(x = implementation) |>
  echarts4r::e_bar(serie = mean) |>
  echarts4r::e_error_bar(lower, upper) |>
  echarts4r::e_grid(
    show = TRUE,
    left = 50,
    right = 50,
    top = 50,
    bottom = 100
  ) |>
  echarts4r::e_tooltip() |>
  echarts4r::e_datazoom(start = 50)
=======
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
  req(data)

  data <- data |>
    subset(ghg_type = ghg_type)

  ggplot(data, aes(x = implementation,
                   y = mean,
                   fill = county)
         ) +
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
      y = paste(fct_label(ghg_type), "(MT CO2e/ac/yr)"),
      title = paste(fct_label(ghg_type))
    ) +
    coord_flip() +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_wrap(x, width = 25)
      }
    ) +
    theme_classic()
>>>>>>> 89a86cc (continued work on table and plot)
}
