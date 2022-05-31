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
#'

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = "county",
      label = "County",
      choices = unique(comet_wa$county),
      multiple = TRUE,
      selected = unique(comet_wa$county[1]),
      options = list(
        plugins = list("remove_button")
      )
    ),
    selectizeInput(
      inputId = "class",
      label = "Conservation Class",
      choices = unique(comet_wa$class),
      multiple = TRUE,
      selected = unique(comet_wa$class[1]),
      options = list(plugins = list("remove_button"))
    ),
    uiOutput("practice"),
    uiOutput("nutrient_practice"),
    uiOutput("land_use"),
    uiOutput("irrigation"),
    actionButton("reset", "Reset All Filters")
  )
}

# Doesn't work when this is in a module.



# mod_filters_server <- function(id) {
#   moduleServer(id = id, function(input, output, session) {
#
#     ns <- session$ns
#
#     fct_makeUI <- function(id, subset, label, num_choice) {
#       output$id <- renderUI({
#         choices <- unique(comet_tags) %>%
#           subset(subset %in% input$subset) %>%
#           select(id)
#
#         choices <- as.character(pull(choices))
#
#         selectizeInput(
#           inputId = id,
#           label = label,
#           choices = choices,
#           selected = choices[num_choice],
#           multiple = TRUE,
#           options = list(
#             maxIttems = 3,
#             plugins = list("remove_button"))
#         )
#       })
#     }
#
#     fct_makeUI(class, practice, "Conservation Practice:", 1)
#
#
#     })
# }
