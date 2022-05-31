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
    br(),
    fluidRow(
      col = 12,
      selectizeInput(
        "ghg_type",
        label = "Select an Emission Reduction Coefficient (ERC) to graph.",
        choices = c(
          "Total Greenhouse Gases" = "total.ghg.co2",
          "Carbon Dioxide" = "co2",
          "Nitrous Oxide" = "n2o",
          "Methane" = "ch4"
        ),
        width = "100%"
      )
    ),
    fluidRow(
      col = 12,
      ggiraph::girafeOutput("plot")
    )
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
