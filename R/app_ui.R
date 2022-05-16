#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd

library(dplyr, warn.conflicts = F)
library(shinyWidgets)

thematic::thematic_shiny(font = "auto")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(version = 5,
                              bootswatch = "lumen",
                              base_font = bslib::font_google("Montserrat")),
      shinyjs::useShinyjs(),
      navbarPage(
        "Washington Climate Smart Estimator (WaCSE)",
        inverse = TRUE,
        collapsible = TRUE,

        tabPanel("Explore the data",
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                mod_selectizeGroup_ui("filters"),
                                br()),
                   mainPanel(width = 9,
                             tabsetPanel(
                               type = "pills",
                               tabPanel("Table",
                                        DT::dataTableOutput("table")
                                        ),
                               tabPanel("Bar Graph",
                                 fluidRow(
                                   selectInput(
                                     "ghg_type",
                                     label = "Select a greenhouse gas to graph.",
                                     choices = c(
                                       "CO2" = "co2",
                                       "N2O" = "n2o",
                                       "Soil Carbon" = "soil.carbon.co2",
                                       "Total GHG" = "total.ghg.co2"
                                     )
                                   )
                                 ),
                                 fluidRow(plotly::plotlyOutput("plot"))
                               )
                             ))
                 )),

        tabPanel("Calculate your estimate"),

        tabPanel("About", includeMarkdown("ABOUT.md"))
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
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(
    favicon(),
    favicon(ext = "png"),
    bundle_resources(path = app_sys("app/www"),
                     app_title = "WaCSEshiny")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
