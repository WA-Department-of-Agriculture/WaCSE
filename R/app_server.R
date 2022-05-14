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
  output$plot <- echarts4r::renderEcharts4r(fct_plot(res_mod()))


}



