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

  if (type == "explore") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            class = "dt-head-left",
            th(rowspan = 2, "MLRA"),
            th(rowspan = 2, "County*"),
            th(rowspan = 2, "Conservation Class"),
            th(rowspan = 2, "Conservation Practice"),
            th(rowspan = 2, "Practice Implementation"),
            th(colspan = 1, "Carbon Dioxide"),
            th(colspan = 1, "Nitrous Oxide"),
            th(colspan = 1, "Methane"),
            th(colspan = 1, "Total GHG"),
          ),
          tr(th(
            colspan = 4,
            class = "dt-head-center",
            em("ERC in MT CO2eq/ac/yr**")
          ))
        )
      )
    )

    selection <- "none"
    hide_targets <- c(0, 2, 5:7)
    rowGrp <- list(dataSrc = 0)
    scroll <- 600
    numeric_cols <- c(6:9)
    acre_col <- 7
    dom <- "Bfrtip"
    order <- 0
    caption <- tags$caption(HTML("*Counties are grouped by their dominant MLRA. <br>
                                  **Blanks mean that ERC was not calculated."))
    filename <- paste0(Sys.Date(), "_WaCSE_exploreTableDownload")
    title <- paste0("Explore table downloaded from WaCSE on ", Sys.Date())
  }

  if (type == "estimate") {
    sketch <- htmltools::withTags(
      table(
        thead(
          tr(
            class = "dt-head-left",
            th(rowspan = 2, "MLRA"),
            th(rowspan = 2, "County*"),
            th(rowspan = 2, "Conservation Class"),
            th(rowspan = 2, "Conservation Practice"),
            th(rowspan = 2, "Practice Implementation"),
            th(rowspan = 2, "Acres"),
            th(colspan = 1, "Carbon Dioxide"),
            th(colspan = 1, "Nitrous Oxide"),
            th(colspan = 1, "Methane"),
            th(colspan = 1, "Total GHG"),
          ),
          tr(th(
            colspan = 4,
            class = "dt-head-center",
            em("Emission reductions in MT CO2eq/yr**")
          ))
        )
      )
    )

    selection <- "multiple"
    hide_targets <- c(0, 2, 6:8)
    rowGrp <- list(dataSrc = 0)
    scroll <- 300
    numeric_cols <- c(7:10)
    acre_col <- 6
    dom <- "Bfrtip"
    order <- 0
    caption <- tags$caption(HTML("*Counties are grouped by their dominant MLRA.<br>
                                 **<b>Emission Reduction Equation:</b>
                                 Area (Acres) × Emission Reduction Coefficient (ERC) <br>
                                 **Blanks mean that ERC was not calculated."))
    filename <- paste0(Sys.Date(), "_WaCSE_estimateTableDownload")
    title <- paste0("Estimate table downloaded from WaCSE on ", Sys.Date())
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
            th("Total GHG Reduction*", br(), em("(MT CO2eq/yr)"))
          )
        )
      )
    )

    selection <- "none"
    hide_targets <- 0
    rowGrp <- NULL
    scroll <- 200
    numeric_cols <- 4:5
    acre_col <- 4
    dom <- "Bfrtip"
    order <- 1
    caption <- tags$caption(HTML("*<b>Total GHG Reduction Equation:</b>
                                  Area (Acres) × Emission Reduction Coefficient (ERC)"))
    filename <- paste0(Sys.Date(), "_WaCSE_estimateSummaryDownload")
    title <- paste0("Estimate summary downloaded from WaCSE on ", Sys.Date())
  }

  DT::datatable(
    data,
    class = "table-compact row-border",
    container = sketch,
    caption = caption,
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
      buttons = list(
        list(
          extend = "excel",
          text = "Download",
          filename = filename,
          title = title
        ),
        list(
          extend = "colvis"
        ),
        list(
          extend = "pageLength"
        )
      ),
      scrollY = scroll,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      order = list(list(
        order, "asc"
      ))
    ),
    selection = selection
  ) %>%
    DT::formatCurrency(columns = numeric_cols, currency = "") %>%
    DT::formatRound(columns = acre_col, digits = 0)
}
