#' editableDT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
# TODO: render datatable

mod_editableDT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("county_E"),
          label = "County",
          choices = unique(comet_wa$county)
        ),
        selectizeInput(
          inputId = ns("class_E"),
          label = "Conservation Class",
          choices = unique(comet_wa$class)
        ),
        uiOutput(ns("practice_E")),
        uiOutput(ns("nutrient_practice_E")),
        uiOutput(ns("land_use_E")),
        uiOutput(ns("irrigation_E")),
        numericInput(
          inputId = ns("acres"),
          label = "Number of Acres",
          value = "1",
          min = 1
        ),
        actionButton(
          inputId = ns("add"),
          label = "Add to table"
        ),
        actionButton(
          inputId = ns("remove"),
          label = "Remove from table"
        )
      ),
      mainPanel(
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}

#' editableDT Server Functions
#'
#' @noRd
mod_editableDT_server <- function(id) {
  moduleServer(id = id, function(input, output, session) {
    ns <- session$ns


    # render UI inputs --------------------------------------------------------

    output$practice_E <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class_E) %>%
        select(practice) %>%
        arrange(practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("practice_E"),
        label = "Conservation Practice",
        choices = choices
      )
    })

    output$land_use_E <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class_E &
          practice %in% input$practice_E) %>%
        select(current_land_use) %>%
        arrange(current_land_use)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("land_use_E"),
        label = "Current Land Use",
        choices = choices
      )
    })

    output$irrigation_E <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice_E) %>%
        select(irrigation) %>%
        arrange(irrigation)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("irrigation_E"),
        label = "Irrigation Type",
        choices = choices
      )
    })

    output$nutrient_practice_E <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice_E)
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice_E) %>%
        select(nutrient_practice) %>%
        arrange(nutrient_practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("nutrient_practice_E"),
        label = "Nutrient Management",
        choices = choices
      )
    })

    observeEvent(input$acres, {
      positive <- input$acres >= 1
      shinyFeedback::feedbackWarning("acres", !positive, "Please select at least one acre.")
      req(input$acres)
      return(input$acres)
    })


    # render table ------------------------------------------------------------
  })
}

## To be copied in the UI
# mod_editableDT_ui("editableDT_1")

## To be copied in the server
# mod_editableDT_server("editableDT_1")
