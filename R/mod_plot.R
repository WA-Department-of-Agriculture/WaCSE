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
    fluidPage(
    #   selectizeInput(
    #     inputId = "ghg_type",
    #     label = "Select an Emission Reduction Coefficient (ERC) to graph.",
    #     choices = c(
    #       "Total Greenhouse Gases" = "total.ghg.co2",
    #       "Carbon Dioxide" = "co2",
    #       "Nitrous Oxide" = "n2o",
    #       "Methane" = "ch4"
    #     ),
    #     width = "100%"
    #   )
    # ),
    # fluidRow(
      ggiraph::girafeOutput("plot", width = "100%")
    )
  )
}

#' plot Server Functions
#'
#' Does not work (Warning: Error in check_aesthetics: Aesthetics must be either length 1 or the same as the data (1): tooltip, x, y, ymin, ymax and fill)
#'
#' @noRd

# mod_plot_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     ghg_type <- reactive({
#       input$ghg_type
#     })
#
#     filtered_df <- mod_filters_server("filters")
#
#     filtered_plot <- reactive({
#       subset(filtered_df(), ghg_type == input$ghg_type)
#     })
#
#     output$plot <- ggiraph::renderGirafe({
#       req(filtered_df(), input$ghg_type)
#       fct_plot(filtered_plot(), ghg_type())
#     })
#   })
# }


## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
