#' make_chart
#'
#' @description A fct function to create a bar graph
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
}
