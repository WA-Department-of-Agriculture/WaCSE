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
#'

fct_plot <- function(data, type, error_bar) {
  # require data
  req(data)

  if (type == "explore") {
    tt <- glue::glue(
      "<b>{data$implementation}</b>
        MLRA: {data$mlra}
        County: {data$county}
        Emission Reduction Coefficient: {data$mean} (MT CO2e/ac/yr)"
    )

    ylab <- paste(
      "\n",
      "Total Greenhouse Gases",
      "\n(Metric tonnes CO2 equivalent per acre per year)"
    )
  }

  if (type == "estimate") {
    tt <- glue::glue(
      "<b>{data$implementation}</b>
        MLRA: {data$mlra}
        County: {data$county}
        Acres: {data$acres}
        Estimated Emission Reduction: {data$mean} (MT CO2e/yr)"
    )

    ylab <- paste(
      "\n",
      "Total Greenhouse Gases",
      "\n(Metric tonnes CO2 equivalent per year)"
    )
  }

  # color blind friendly colors for negative-bad, positive-good
  # x_axis_cols <- ifelse(data$mean > 0, "#018571", "#a6611a")

  # plot data
  plot <-
    ggplot(
      data,
      aes(
        x = factor(fct_wrap(abbr, 30)),
        y = mean,
        fill = factor(fct_wrap(mlra, 20)),
      )
    ) +
    coord_flip() +
    geom_col_interactive(aes(
      tooltip = tt
    ),
    position = position_dodge2(reverse = TRUE),
    na.rm = TRUE
    ) +
    scale_fill_viridis_d() +
    labs(
      fill = "MLRA Legend",
      x = NULL,
      y = ylab
    ) +
    theme_classic(base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(
        margin = margin(r = 20),
        hjust = 0
      ),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(margin = margin(t = 5, b = 5, unit = "pt"))
      # axis.text.x = element_text(color = x_axis_cols) not supported in girafe
    ) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.001),
      expand = expansion(mult = c(0.03, 0.3))
    )

  # add error bars if error_bar = TRUE

  if (error_bar == TRUE) {
    plot <- plot +
      geom_errorbar(aes(
        ymin = lower,
        ymax = upper
      ),
      position = position_dodge2(
        width = 0.01,
        padding = 0.1,
        reverse = TRUE
      )
      ) +
      shadowtext::geom_shadowtext(aes(
        y = ifelse(!is.na(upper), upper, mean),
        label = county
      ),
      hjust = -0.1,
      color = "black",
      fontface = "bold",
      bg.color = "white",
      bg.r = 0.1,
      position = position_dodge2(width = 0.9, reverse = TRUE),
      na.rm = TRUE
      )
  }

  if (error_bar == FALSE) {
    plot <- plot +
      shadowtext::geom_shadowtext(aes(
        label = county
      ),
      hjust = -0.05,
      color = "black",
      fontface = "bold",
      bg.color = "white",
      bg.r = 0.1,
      position = position_dodge2(width = 0.9, reverse = TRUE),
      na.rm = TRUE
      )
  }

  # plot with ggiraph

  tooltip_css <- "font-size:1rem; color:black; background:white; padding:8px; border-radius:6px;"

  plot <- girafe(
    ggobj = plot,
    width_svg = 9,
    options = list(
      opts_tooltip(
        css = tooltip_css
      ),
      opts_toolbar(saveaspng = TRUE, pngname = paste(Sys.Date(), "_WaCSE_Plot"))
    )
  )
}
