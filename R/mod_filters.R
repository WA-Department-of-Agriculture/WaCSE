#' filters UI Function
#'
#' @description A shiny Module for filtering the comet data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = "county",
      label = "County:",
      choices = unique(comet_wa$county),
      multiple = TRUE,
      selected = unique(comet_wa$county[1])
    ),
    selectInput(
      inputId = "class",
      label = "Conservation Class:",
      choices = unique(comet_wa$class),
      multiple = TRUE,
      selected = unique(comet_wa$class[1])
    ),
    uiOutput("practice"),
    uiOutput("irrigation"),
    actionButton("reset", "Reset")
  )
}

# Doesn't work when this is in a module.

# mod_filters_server <- function(id) {
#   moduleServer(id = id, function(input, output, session) {
#
#     ns <- session$ns
#
#     output$practice <- renderUI({
#       choices <- unique(comet_wa
#                         [comet_wa$class == input$class, "practice"])
#       choices <- as.character(pull(choices))
#
#       selectInput(
#         inputId = "practice",
#         label = "Conservation Practice:",
#         choices = choices,
#         selected = choices[1],
#         multiple = TRUE
#       )
#     })
#
#     output$irrigation <- renderUI({
#       choices <- unique(comet_wa
#                         [comet_wa$practice == input$practice, "irrigation"])
#       choices <- as.character(pull(choices))
#
#       selectInput(
#         inputId = "irrigation",
#         label = "Irrigation Type:",
#         choices = choices,
#         selected = choices,
#         multiple = TRUE
#       )
#     })
#
#     observeEvent(input$reset, {
#       updateSelectInput(session, inputId = "county", selected = "")
#       updateSelectInput(session, inputId = "class", selected = "")
#       updateSelectInput(session, inputId = "practice", selected = "")
#       updateSelectInput(session, inputId = "irrigation", selected = "")
#     })
#   })
# }
