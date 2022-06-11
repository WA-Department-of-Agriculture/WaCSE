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
            class = "dt-head-center",
            tags$i("(Metric tonnes CO2 equivalent per acre per year)")
          ))
        )
      )
    )

    selection <- "none"
    hide_targets <- 0:2
    targets <- 0:8
    rowGrp <- list(dataSrc = 0:1)
  }

  if (type == "estimate") {
    sketch <- htmltools::withTags(
      table(
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
            class = "dt-head-center",
            tags$i("(Metric tonnes CO2 equivalent per acre per year)")
          ))
        )
      )
    )

    selection <- "multiple"
    hide_targets <- 0:2
    targets <- 0:9
    rowGrp <- list(dataSrc = 0:1)
  }

  if (type == "summary_county") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            th(rowspan = 2, "MLRA"),
            th(rowspan = 2, "County"),
            th(rowspan = 2, "Acres"),
            th("Total Greenhouse Gases")
          ),
          tr(
            th(
              colspan = 1,
              tags$i("(MT CO2eq/ac/yr)")
            )
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- 0
    targets <- 0:3
    rowGrp <- NULL
  }

  if (type == "summary_practice") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            th(rowspan = 2, "Practice"),
            th(rowspan = 2, "Practice Implementation"),
            th(rowspan = 2, "Acres"),
            th("Total Greenhouse Gases")
          ),
          tr(
            th(
              colspan = 1,
              tags$i("(MT CO2eq/ac/yr)")
            )
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- 0
    targets <- 0:3
    rowGrp <- list(dataSrc = 0)
  }


  DT::datatable(
    data,
    class = "compact row-border",
    container = sketch,
    rownames = FALSE,
    extensions = c("Buttons", "Scroller", "RowGroup", "FixedHeader"),
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(
          class = "dt-head-left",
          targets = targets
        ),
        list(
          visible = FALSE,
          targets = hide_targets
        )
      ),
      dom = "B, t",
      rowGroup = rowGrp,
      pageLength = 500,
      buttons = list(list(
        extend = "collection",
        buttons = list(
          list(extend = "csv", filename = "WaCSE_download"),
          list(extend = "excel", filename = "WacSE_download"),
          list(extend = "pdf", filename = "WaCSE_download")
        ),
        text = "Download"
      ))
    ),
    selection = selection
  )
}
