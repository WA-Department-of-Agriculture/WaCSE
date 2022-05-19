#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      "ghg_type",
      label = "Select a greenhouse gas to graph.",
      choices = c(
        "CO2" = "co2",
        "N2O" = "n2o",
        "Soil Carbon" = "soil.carbon.co2",
        "Total GHG" = "total.ghg.co2"
      )
    ),
    (ggiraph::girafeOutput("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd


# mod_plot_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     ghg_type <- reactive({
#       input$ghg_type
#     })
#
#     output$plot <-
#       echarts4r::renderEcharts4r(fct_plot(data, ghg_type))
#   })
# }


## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
