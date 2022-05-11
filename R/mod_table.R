#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

col_names <-
  c(
    "County",
    "Class",
    "Practice",
    "Implementation",
    "CO2 Mean",
    "N2O Mean",
    "Soil Carbon Mean",
    "Total GHG"
  )

mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
      DT::dataTableOutput(ns("explore"))
      )
  }

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
    ns <- session$ns

    DT::renderDataTable({
    data %>%
      select(county,
             class,
             cps_name,
             planner_implementation,
             co2_mean,
             n2o_mean,
             soil_carbon_co2,
             total_ghg_co2)
    })
  })
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
