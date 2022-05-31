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
    dplyr::mutate(mean = tidyr::replace_na(as.character(mean), "Not estimated")) |>
    tidyr::pivot_wider(
      names_from = ghg_type,
      values_from = mean
    )


  sketch <- htmltools::withTags(
    table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "County"),
          th(rowspan = 2, "Conservation Class"),
          th(rowspan = 2, "Conservation Practice"),
          th(rowspan = 2, "Practice Implementation"),
          th(colspan = 1, "Carbon Dioxide"),
          th(colspan = 1, "Nitrous Oxide"),
          th(colspan = 1, "Methane"),
          th(colspan = 1, "Total Greenhouse Gases"),
        ),
        tr(th(
          colspan = 4,
          tags$i("(metric tonnes CO2 equivalent per acre per year)")
        ))
      )
    )
  )

  DT::datatable(
    data,
    class = "table-condensed",
    container = sketch,
    rownames = FALSE,
    extensions = c("Buttons", "Scroller", "RowGroup"),
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(
        className = "dt-head-center",
        targets = 0:7
      )),
      dom = "B, t, p",
      rowGroup = list(dataSrc = 0),
      pageLength = 10,
      buttons = list(list(
        extend = "collection",
        buttons = c("csv", "excel"),
        text = "Download"
      )),
      scrollX = 400,
      scrollY = 400
    ),
    selection = "none"
  )
}
