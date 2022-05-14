#' selectizeGroup UI Function
#'
#' @description A shiny Module for filtering the comet data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selectizeGroup_ui <- function(id) {
  ns <- NS(id)
  tagList(selectizeGroupUI(
    id = "filters",
    params = list(
      county = list(
        inputId = "county",
        label = "County:",
        placeholder = "All counties"
      ),
      class = list(
        inputId = "class",
        label = "Conservation Class:",
        placeholder = "All classes"
      ),
      practice = list(
        inputId = "practice",
        label = "Conservation practices:",
        placeholder = "All practices"
      ),
      irrigation = list(
        inputId = "irrigation",
        label = "Irrigation Type:",
        placeholder = "All irrigation types"
      )
    ),
    inline = FALSE
  ))
}

#' selectizeGroup Server Functions
#'
#' @noRd
#'
#' this doesn't work for passing the resulting filtered dataframe to other outputs
#'
# mod_selectizeGroup_server <- function(id) {
#   id
#     moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     selectizeGroupServer(
#       input,
#       output,
#       session,
#       data = comet_wa,
#       vars = c("county", "class", "practice", "irrigation"),
#       inline = FALSE
#     )
#   })
# }

## To be copied in the UI
# mod_selectizeGroup_ui("selectizeGroup_1")

## To be copied in the server
# mod_selectizeGroup_server("selectizeGroup_1")
