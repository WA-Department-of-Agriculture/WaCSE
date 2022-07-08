#' land_use UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_land_use_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 12, status = "primary",
      htmlOutput(ns("crop_map"))
    ),
  )
}

#' land_use Server Functions
#'
#' @noRd
mod_land_use_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ArcGIS crops iframe

    output$crop_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/dashboards/260347f5c6884d00b51b8192745f0751",
        height = "600px",
        width = "100%"
      )
    })
  })
}
## To be copied in the UI
# mod_land_use_ui("land_use_1")

## To be copied in the server
# mod_land_use_server("land_use_1")
