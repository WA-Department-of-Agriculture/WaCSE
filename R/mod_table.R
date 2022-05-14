#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
      DT::dataTableOutput(ns("table"))
      )
  }

#' table Server Functions
#'
#' @noRd
#'
mod_table_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$table <- DT::renderDataTable(
        data %>%
          select_cols() %>%
          rename_cols(),
        rownames = FALSE,
        extensions = c("Scroller", "RowGroup"),
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = "10px", targets = "_all")),
          dom = "t, p",
          rowGroup = list(dataSrc = 0),
          pageLength = 10,
          scrollX = TRUE,
          scrollY = 400
        ),
        selection = "none"
      )
    }
  )
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
