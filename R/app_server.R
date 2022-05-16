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
    vars = c("county", "class", "practice", "irrigation"),
    inline = FALSE
  )


  output$table <- DT::renderDataTable(fct_table(res_mod()))

  ghg_type <- reactive({input$ghg_type})

  filtered <- reactive({
    subset(res_mod(), ghg_type == input$ghg_type)
  })


  output$plot <- plotly::renderPlotly(fct_plot(filtered(), ghg_type()))


}



