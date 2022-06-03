#' filters UI Function
#'
#' @description A shiny Module for filtering the comet data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @return reactive filtered df

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("county"),
      label = "County",
      choices = unique(comet_wa$county),
      multiple = TRUE,
      selected = unique(comet_wa$county[1]),
      options = list(
        plugins = list("remove_button")
      )
    ),
    selectizeInput(
      inputId = ns("class"),
      label = "Conservation Class",
      choices = unique(comet_wa$class),
      multiple = TRUE,
      selected = unique(comet_wa$class[1]),
      options = list(plugins = list("remove_button"))
    ),
    uiOutput(ns("practice")),
    uiOutput(ns("nutrient_practice")),
    uiOutput(ns("land_use")),
    uiOutput(ns("irrigation")),
  )
}

mod_filters_server <- function(id) {
  moduleServer(id = id, function(input, output, session) {
    ns <- session$ns

    output$practice <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class) %>%
        select(practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("practice"),
        label = "Conservation Practice",
        choices = choices,
        selected = choices[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$land_use <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(current_land_use)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("land_use"),
        label = "Current Land Use",
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
        inputId = ns("irrigation"),
        label = "Irrigation Type",
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$nutrient_practice <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice)
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice) %>%
        select(nutrient_practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("nutrient_practice"),
        label = "Nutrient Management",
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

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
  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
