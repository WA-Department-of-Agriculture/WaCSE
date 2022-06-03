#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("table"))
  )
}

#' table Server Functions
#'
#' @noRd
#'
mod_table_server <- function(id) {
  moduleServer(
    id = id,
    function(input, output, session) {

      filtered_df <- mod_filters_server("filters")

      output$table <- DT::renderDataTable(fct_table(filtered_df()))
    }
  )
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
