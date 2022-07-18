#'  impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    )
}

#' impact Server Functions
#'
#' @noRd
mod_impact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
## To be copied in the UI
# mod_impact_ui("impact_1")

## To be copied in the server
# mod_impacte_server("impact_1")
