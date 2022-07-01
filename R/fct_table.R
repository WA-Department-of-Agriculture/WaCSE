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
            class = "dt-head-left",
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
            em("(Metric tonnes CO2 equivalent per acre per year)")
          ))
        )
      )
    )

    selection <- "none"
    hide_targets <- 0:2
    targets <- 0:8
    rowGrp <- list(dataSrc = 0:1)
    scroll <- 500
  }

  if (type == "estimate") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            class = "dt-head-left",
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
            em("(Metric tonnes CO2 equivalent per year)")
          ))
        )
      )
    )

    selection <- "multiple"
    hide_targets <- 0:2
    targets <- 0:9
    rowGrp <- list(dataSrc = 0:1)
    scroll <- 200
  }

  if (type == "summary") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            class = "dt-head-left",
            th("MLRA"),
            th("County"),
            th("Unique Practice Implementations"),
            th("Total Acres"),
            th("Total GHG", em("(MT CO2eq/yr)"))
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- 0
    targets <- 0:4
    rowGrp <- NULL
    scroll <- 200
  }

  DT::datatable(
    data,
    class = "table-compact row-border",
    container = sketch,
    rownames = FALSE,
    extensions = c("Buttons", "Scroller", "RowGroup", "FixedHeader"),
    options = list(
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
      dom = "B,t,p",
      rowGroup = rowGrp,
      pageLength = 25,
      buttons = list(list(
        extend = "collection",
        buttons = list(
          list(
            extend = "csv", filename = paste(Sys.Date(), "_WaCSE_Download"),
            title = paste("Downloaded from WaCSE on", Sys.Date())
          ),
          list(
            extend = "excel", filename = paste(Sys.Date(), "_WacSE_Download"),
            title = paste("Downloaded from WaCSE on", Sys.Date())
          ),
          list(
            extend = "pdf", filename = paste(Sys.Date(), "_WaCSE_Download"),
            title = paste("Downloaded from WaCSE on", Sys.Date())
          )
        ),
        text = "Download"
      )),
      scrollY = scroll,
      scrollX = TRUE,
      scrollCollapse = TRUE
    ),
    selection = selection
  )
}
