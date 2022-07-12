#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import waiter
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = theme,
      navbarPage(
        "Washington Climate Smart Estimator (WaCSE)",
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
          "Agricultural land use",
          mod_land_use_ui("land_use_tab")
        ),
        tabPanel(
          "About",
          column(8,
            offset = 1,
            box(width = NULL, status = "success", includeMarkdown("ABOUT.md"))
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

  tags$head(
    favicon(),
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WaCSE"
    ),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::setBackgroundColor(color = "#dfdfdf"),
    waiter::useWaiter(),
    waiter::waiterPreloader(html = spin_hexdots())

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
