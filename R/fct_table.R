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
            th("MLRA"),
            th("County"),
            th("Conservation Class"),
            th("Conservation Practice"),
            th("Practice Implementation"),
            th("Carbon Dioxide"),
            th("Nitrous Oxide"),
            th("Methane"),
            th("Total GHG", br(), em("(MT CO2eq/yr)")),
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- c(0:2, 5:7)
    targets <- 0:8
    rowGrp <- list(dataSrc = 0:1)
    scroll <- 500
    numeric_cols <- c(6, 9)
    acre_col <- 1
    dom <- "Blfrtip"
  }

  if (type == "estimate") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            class = "dt-head-left",
            th("MLRA"),
            th("County"),
            th("Conservation Class"),
            th("Conservation Practice"),
            th("Practice Implementation"),
            th("Acres"),
            th("Carbon Dioxide"),
            th("Nitrous Oxide"),
            th("Methane"),
            th("Total GHG", br(), em("(MT CO2eq/yr)")),
          )
        )
      )
    )

    selection <- "multiple"
    hide_targets <- c(0:2, 6:8)
    targets <- 0:9
    rowGrp <- list(dataSrc = 0:1)
    scroll <- 200
    numeric_cols <- c(6:7, 10)
    acre_col <- 6
    dom <- "Blfrtip"
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
            th("Total GHG", br(), em("(MT CO2eq/yr)"))
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- 0
    targets <- 0:4
    rowGrp <- NULL
    scroll <- 200
    numeric_cols <- 4:5
    acre_col <- 4
    dom <- "lfrtip"
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
          visible = FALSE,
          targets = hide_targets
        )
      ),
      dom = dom,
      rowGroup = rowGrp,
      buttons = list(list(
        extend = "collection",
        buttons = list(
          list(
            extend = "csv", filename = paste0(Sys.Date(), "_WaCSE_Download"),
            title = paste("Downloaded from WaCSE on", Sys.Date())
          ),
          list(
            extend = "excel", filename = paste0(Sys.Date(), "_WaCSE_Download"),
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
  ) %>%
    DT::formatCurrency(columns = numeric_cols, currency = "") %>%
    DT::formatRound(columns = acre_col, digits = 0)
}
