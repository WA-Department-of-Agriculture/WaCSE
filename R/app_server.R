#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# TODO: keep trying to modularize the code and figure out why the server parts don't work

app_server <- function(input, output, session) {
  # Your application server logic

  # Render UI filter elements -----------------------------------------------

  output$practice <- renderUI({
    choices <- unique(comet_tags) %>%
      subset(class %in% input$class) %>%
      select(practice)

    choices <- as.character(pull(choices))

    selectizeInput(
      inputId = "practice",
      label = "Conservation Practice:",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output$land_use <- renderUI({
    choices <- unique(comet_tags) %>%
      subset(practice %in% input$practice) %>%
      select(current_land_use)

    choices <- as.character(pull(choices))

    selectizeInput(
      inputId = "land_use",
      label = "Current Land Use:",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output$irrigation <- renderUI({
    choices <- unique(comet_tags) %>%
      subset(practice %in% input$practice) %>%
      select(irrigation)

    choices <- as.character(pull(choices))

    selectizeInput(
      inputId = "irrigation",
      label = "Irrigation Type:",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output$nutrient_practice <- renderUI({
    req(input$practice %in% "Nutrient Management (CPS 590)")
    choices <- unique(comet_tags) %>%
      subset(practice %in% input$practice) %>%
      select(nutrient_practice)

    choices <- as.character(pull(choices))

    selectizeInput(
      inputId = "nutrient_practice",
      label = "Nutrient Management:",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  observeEvent(input$reset, {
    updateSelectizeInput(session, inputId = "county", selected = "")
    updateSelectizeInput(session, inputId = "class", selected = "")
    updateSelectizeInput(session, inputId = "practice", selected = "")
    updateSelectizeInput(session, inputId = "land_use", selected = "")
    updateSelectizeInput(session, inputId = "irrigation", selected = "")
    updateSelectizeInput(session, inputId = "nutrient_practice", selected = "")
  })


  # Generate reactive filtered dataframe ------------------------------------

  filtered_df <- reactive({
    if (!("Nutrient Management (CPS 590)" %in% input$practice)) {
      subset(
        comet_wa,
        county %in% input$county &
          class %in% input$class &
          practice %in% input$practice &
          current_land_use %in% input$land_use &
          irrigation %in% input$irrigation
      )
    } else {
      subset(
        comet_wa,
        county %in% input$county &
          class %in% input$class &
          practice %in% input$practice &
          current_land_use %in% input$land_use &
          irrigation %in% input$irrigation &
          nutrient_practice %in% input$nutrient_practice
      )
    }
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

  output$plot <- ggiraph::renderGirafe({
    req(filtered_plot())
    fct_plot(filtered_plot(), ghg_type())
  })
}
