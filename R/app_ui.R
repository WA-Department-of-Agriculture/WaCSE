#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#'
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = theme,
      navbarPage(
        title = span(
          img(src = "www/rmd/img/favicon.png", height = "80px"),
          "Washington Climate Smart Estimator"
        ),
        collapsible = TRUE,
        tabPanel(
          "Explore the data",
          mod_explore_ui("explore_tab")
        ),
        tabPanel(
          "Calculate your estimate",
          mod_estimate_ui("estimate_tab")
        ),
        tabPanel(
          "Understand your impact",
          mod_impact_ui("impact_tab")
        ),
        tabPanel(
          "About",
          column(10,
            offset = 1,
            shinydashboard::box(
              width = NULL, status = "primary",
              includeMarkdown(normalizePath("inst/app/www/rmd/about.md"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  source("R/globals.R")

  tags$head(
    favicon(),
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WaCSE"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/css/style.css"
    ),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::setBackgroundColor(color = "#faf9f0"),
    waiter::useWaiter(),
    waiter::waiterPreloader(html = waiter::spin_hexdots(), fadeout = TRUE),
    shinydisconnect::disconnectMessage(
      text = "Your session timed out. Please refresh the page and try again.",
      refresh = "Refresh",
      background = "#FFFFFF",
      colour = "#444444",
      refreshColour = "#337AB7",
      overlayColour = "#000000",
      overlayOpacity = 0.6,
      width = 450,
      top = "center",
      size = 22,
      css = ""
    )

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
# spinner options
options(spinner.type = 7, spinner.color = "#81B29A", spinner.size = 1.5)
