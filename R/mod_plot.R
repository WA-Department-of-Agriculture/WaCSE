#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id, var){
  ns <- NS(id)
  tagList(
    tabPanel(

    )

  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
