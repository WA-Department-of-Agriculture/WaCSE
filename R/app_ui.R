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
        title = span(img(src = "www/rmd/img/favicon.png", height = "80px"),
                     "Washington Climate Smart Estimator"),
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
          column(8,
            offset = 1,
            box(width = NULL, status = "primary",
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

  tags$head(
    favicon(),
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WaCSE"
    ),
    # this doesn't work in the style.css file. works in this ui file.
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #333;
        font-weight: bold;
      }
    ")
    ),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::setBackgroundColor(color = "#faf9f0"),
    bsplus::use_bs_popover(),
    waiter::useWaiter(),
    waiter::waiterPreloader(html = spin_hexdots())

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
