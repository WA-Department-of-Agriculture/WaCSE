#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  # intro modal -------------------------------------------------------------
  observeEvent("", {
    fct_helpModal(md = "intro")
  })

  # render explore tab ------------------------------------------------------

  mod_explore_server("explore_tab")

  # render estimate tab ------------------------------------------------------

  mod_estimate_server("estimate_tab")

  # render understand tab --------------------------------------------------------------

  mod_impact_server("impact_tab")

  # stop session when browser is closed -------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
