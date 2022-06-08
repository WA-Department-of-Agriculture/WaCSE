#' make_table
#'
#' @description A fct function to create a datatable
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'

fct_table <- function(data, type) {
  req(data)

  type <- type

  if (type == "explore") {
    sketch <- htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "MLRA"),
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

    selection <- "none"
    targets <- 0:8
  }

  if (type == "estimate") {
    sketch <- htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "MLRA"),
            th(rowspan = 2, "County"),
            th(rowspan = 2, "Conservation Class"),
            th(rowspan = 2, "Conservation Practice"),
            th(rowspan = 2, "Practice Implementation"),
            th(rowspan = 2, "Acres"),
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

    selection <- "multiple"
    targets <- 0:9
  }

  DT::datatable(
    data,
    class = "table-condensed",
    container = sketch,
    rownames = FALSE,
    extensions = c("Buttons", "Scroller", "RowGroup", "Responsive"),
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(
          class = "dt-head-left",
          targets = targets
        ),
        list(
          class = "dt-right",
          targets = targets
        ),
        list(
          visible = FALSE,
          targets = 0:1
        )
      ),
      dom = "B, t, p",
      rowGroup = list(dataSrc = 0:1),
      pageLength = 100,
      buttons = list(list(
        extend = "collection",
        buttons = list(
          list(extend = "csv", filename = "WaCSE_download"),
          list(extend = "excel", filename = "WacSE_download"),
          list(extend = "pdf", filename = "WaCSE_download")
        ),
        text = "Download"
      )),
      scrollX = 400,
      scrollY = 400
    ),
    selection = selection
  )
}
