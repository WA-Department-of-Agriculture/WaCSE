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
        <b>MLRA:</b> {data$mlra}
        <b>County:</b> {data$county}
        <b>Emission Reduction Coefficient:</b> {data$mean} (MT CO2e/ac/yr)"
    )

    ylab <- paste(
      "\n",
      "Total Greenhouse Gases",
      "\n(Metric tonnes CO2 equivalent per acre per year)"
    )

    font <- "Poppins"

    title <- NULL

    labels <- scales::label_number(accuracy = 0.001, big.mark = ",")
  }

  if (type == "estimate") {
    tt <- glue::glue(
      "<b>{data$implementation}</b>
        <b>MLRA:</b> {data$mlra}
        <b>County:</b> {data$county}
        <b>Acres:</b> {format(round(data$acres, 0), big.mark = ',')}
        <b>Estimated Emission Reduction:</b> {format(round(data$mean, 2), big.mark = ',')} (MT CO2e/yr)"
    )

    ylab <- paste(
      "\n",
      "Total Greenhouse Gases",
      "\n(Metric tonnes CO2 equivalent per year)"
    )

    font <- "Poppins"

    title <- NULL

    labels <- scales::label_number(accuracy = 1, big.mark = ",")
  }

  if (type == "download") {
    tt <- glue::glue(
      "<b>{data$implementation}</b>
        <b>MLRA:</b> {data$mlra}
        <b>County:</b> {data$county}
        <b>Acres:</b> {format(round(data$acres, 0), big.mark = ',')}
        <b>Estimated Emission Reduction:</b> {format(round(data$mean, 2), big.mark = ',')} (MT CO2e/yr)"
    )

    ylab <- paste(
      "\n",
      "Total Greenhouse Gases",
      "\n(Metric tonnes CO2 equivalent per year)"
    )

    font <- "Arial"

    title <- "Figure 1: Total GHG Emission Reductions"

    labels <- scales::label_number(accuracy = , big.mark = ",")
  }

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
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_viridis_d(option = "F", end = 0.9) +
    labs(
      fill = "MLRA Legend",
      x = NULL,
      y = ylab
    ) +
    theme_minimal(base_family = font) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 10),
      axis.text.y = element_text(
        margin = margin(r = 20),
        hjust = 0
      ),
      axis.title.x.bottom = element_text(
        size = 9,
        color = "#4d4d4d"
      ),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(margin = margin(t = 5, b = 5, unit = "pt"))
    ) +
    scale_y_continuous(
      labels = labels,
      expand = expansion(mult = c(0.03, 0.3))
    ) +
    ggtitle(title)

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

  tooltip_css <- "font-size:1rem; color:black; background:#FAF9F0; padding:8px; border-radius:6px;"

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
