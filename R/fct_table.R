#' make_table
#'
#' @description A fct function to create a datatable
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'

fct_table <- function(data) {
  req(data)
    data <-
      dplyr::select(
        data,
        "county",
        "class",
        "practice",
        "implementation",
        "ghg_type",
        "mean"
      ) |>
      tidyr::pivot_wider(names_from = ghg_type,
                         values_from = mean)

    sketch <- htmltools::withTags(
      table(class = 'display',
            thead(
              tr(
                th(rowspan = 2, "County"),
                th(rowspan = 2, "Conservation Class"),
                th(rowspan = 2, "Conservation Practice"),
                th(rowspan = 2, "Practice Implementation"),
                th(colspan = 1, "Carbon Dioxide"),
                th(colspan = 1, "Nitrous Oxide"),
                th(colspan = 1, "Soil Carbon"),
                th(colspan = 1, "Total Greenhouse Gases"),
              ),
              tr(th(colspan = 4,
                    tags$i("(metric tonnes CO2 equivalent per acre per year)")))
            )))

    DT::datatable(
      data,
      container = sketch,
      rownames = FALSE,
      extensions = c("Scroller", "RowGroup"),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(
          width = "50px", targets = 4:7
        )),
        dom = "t, p",
        rowGroup = list(dataSrc = 0),
        pageLength = 10,
        scrollX = FALSE,
        scrollY = 500
      ),
      selection = "none"
    )
}
