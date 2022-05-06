#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  data <- reactive(comet_wa)

  filterSet <- initializeFilterSet(filterSet, data)

  output$table <- DT::renderDataTable(filterSet$output())

  }
