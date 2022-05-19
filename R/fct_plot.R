#' make_chart
#'
#' @description A fct function to create a bar graph
#'
#' @import ggplot2
#' @import ggiraph
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

# TODO:   fix error messages if no filters are selected
#         add conditional UI selector for nutrient management

fct_plot <- function(data, ghg_type) {
  # require data
  req(data)

  # subset data based on chosen ghg type
  data <- data |>
    subset(ghg_type = ghg_type)

  # plot data
  plot <-
    ggplot(
      data,
      aes(
        x = factor(wrap(implementation, 100)),
        y = mean,
        ymin = lower,
        ymax = upper,
        fill = county,
        text = paste(
          "Practice Implementation: ",
          implementation,
          "\nCounty: ",
          county,
          "\nGHG Coefficient: ",
          mean
        ),
        na.rm = TRUE
      )
    ) +
    coord_flip() +
    geom_col_interactive(aes(tooltip = glue::glue("<b>{implementation}</b>\nCounty: {county}\nGHG Estimate: {mean} (MT CO2e/ac/yr)")),
                         position = position_dodge2(reverse = TRUE)
                         ) +
    geom_errorbar(
      position = position_dodge2(
        width = 0.01,
        padding = 0.1,
        reverse = TRUE
      )
    ) +
    scale_fill_viridis_d(begin = 0,
                         end = 0.8) +
    labs(
      fill = "County",
      x = NULL,
      y = paste("\n",
                fct_label(ghg_type),
                "\n(Metric tonnes CO2 equivalent per acre per year)")
    ) +
    theme_classic(base_family = "montserrat") +
    theme(axis.text.y = element_text(margin = margin(r = 20)))

  # plot with ggiraph

  tooltip_css <- "color:white;outline-color:black;padding:8px;border-radius:6px;"

  plot <- girafe(ggobj = plot,
                 width_svg = 10,
                 height_svg = 4,
                 options = list(
                   opts_tooltip(css = tooltip_css,
                                use_fill = TRUE),
                   opts_toolbar(pngname = "plot")
                   )
  )

}
