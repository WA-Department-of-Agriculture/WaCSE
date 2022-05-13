#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "filters",
    data = comet_wa,
    vars = c("county", "class", "cps_name", "irrigation"),
    inline = FALSE
  )


  output$table <- DT::renderDataTable(table(res_mod()))
  output$plot_1 <- renderPlot(bar_graph(res_mod(), y = res_mod()$co2_mean))


}



