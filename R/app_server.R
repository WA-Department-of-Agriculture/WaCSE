#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  # Render UI filter elements -----------------------------------------------

  output$practice <- renderUI({
    choices <- unique(comet_wa
                      [comet_wa$class == input$class, "practice"])
    choices <- as.character(pull(choices))

    selectInput(
      inputId = "practice",
      label = "Conservation Practice:",
      choices = choices,
      selected = choices[1],
      multiple = TRUE
    )
  })

  output$irrigation <- renderUI({
    choices <- unique(comet_wa
                      [comet_wa$practice == input$practice, "irrigation"])
    choices <- as.character(pull(choices))

    selectInput(
      inputId = "irrigation",
      label = "Irrigation Type:",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })

  observeEvent(input$reset, {
    updateSelectInput(session, inputId = "county", selected = "")
    updateSelectInput(session, inputId = "class", selected = "")
    updateSelectInput(session, inputId = "practice", selected = "")
    updateSelectInput(session, inputId = "irrigation", selected = "")
  })


  # Generate reactive filtered dataframe ------------------------------------

  filtered_df <- reactive({
    subset(
      comet_wa,
      comet_wa$county %in% input$county &
        comet_wa$class %in% input$class &
        comet_wa$practice %in% input$practice &
        comet_wa$irrigation %in% input$irrigation
    )
  })


  # Render table ------------------------------------------------------------

  output$table <- DT::renderDataTable(fct_table(filtered_df()))


  # Render plot -------------------------------------------------------------

  ghg_type <- reactive({
    input$ghg_type
  })

  filtered_plot <- reactive({
    subset(filtered_df(), ghg_type == input$ghg_type)
  })

  output$plot <- plotly::renderPlotly({
    fct_plot(filtered_plot(), ghg_type())
  })

}
