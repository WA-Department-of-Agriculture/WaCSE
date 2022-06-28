#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  # render explore tab ------------------------------------------------------

  mod_explore_server("explore_tab")

  # render estimate tab ------------------------------------------------------

  mod_estimate_server("estimate_tab")

  # render ag land use tab --------------------------------------------------------------

  mod_land_use_server("land_use_tab")

  # render download tab -----------------------------------------------------

  mod_download_server("download_tab")
}
