#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import shinycssloaders
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
        inverse = TRUE,
        collapsible = TRUE,
        tabPanel(
          "Explore the data",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              style = "overflow: auto; max-height: 675px; position: relative;",
              div(
                id = "form",
                mod_filters_ui("filters")
              ),
              actionButton("reset", "Reset Selection")
            ),
            mainPanel(
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Table",
                  br(),
                  withSpinner(DT::DTOutput("table", width = "100%"))
                ),
                tabPanel(
                  "Bar Graph",
                  withSpinner(ggiraph::girafeOutput("plot", width = "100%"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Calculate your estimate",
          mod_editableDT_ui("editableDT")
        ),
        tabPanel(
          "Agricultural Land Use",
          mod_land_use_ui("land_use_tab")
        ),
        tabPanel(
          "About",
          column(8,
            offset = 1,
            includeMarkdown("ABOUT.md")
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
      app_title = "WaCSEshiny"
    ),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

# set global spinner options

options(spinner.type = 5, spinner.color = "#489739")
