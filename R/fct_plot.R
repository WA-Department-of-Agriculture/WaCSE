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

fct_plot <- function(data, ghg_type, error_bar) {
  # require data
  req(data)

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
      tooltip = glue::glue(
        "<b>{implementation}</b>
        MLRA: {mlra}
        County: {county}
        Emission Reduction Coefficient: {mean} (MT CO2e/ac/yr)"
      )
    ),
    position = position_dodge2(reverse = TRUE),
    na.rm = TRUE
    ) +
    scale_fill_viridis_d() +
    labs(
      title = "Total Greenhouse Gas Emission Reduction Coefficients",
      fill = "MLRA Legend",
      x = NULL,
      y = paste(
        "\n",
        fct_label(ghg_type),
        "\n(Metric tonnes CO2 equivalent per acre per year)"
      )
    ) +
    theme_classic(base_family = "poppins") +
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
      hjust = -0.1,
      color = "black",
      fontface = "bold",
      bg.color = "white",
      bg.r = 0.1,
      position = position_dodge2(width = 0.9, reverse = TRUE),
      na.rm = TRUE)
  }

  # plot with ggiraph

  tooltip_css <- "font-size:0.8rem; color:black; background:white; padding:8px; border-radius:6px;"

  plot <- girafe(
    ggobj = plot,
    width_svg = 12,
    height_svg = 5,
    options = list(
      opts_tooltip(
        css = tooltip_css
      ),
      opts_toolbar(saveaspng = FALSE),
      opts_zoom(max = 5)
    )
  )
}
