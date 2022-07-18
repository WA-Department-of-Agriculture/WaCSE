#'  impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box valueBoxOutput renderValueBox valueBox

mod_impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12, status = "primary",
        includeMarkdown(normalizePath("inst/app/www/rmd/aboutGHGeq.md")),
        HTML("<h4><strong>500 metric tonnes of CO2eq</strong>
             is equivalent to CO2 emissions from:</h4>"),
        fluidRow(
          valueBoxOutput(ns("home")),
          valueBoxOutput(ns("gallons")),
          valueBoxOutput(ns("phones"))
        ),
        fluidRow(
          HTML("<h4><strong>500 metric tonnes of CO2eq</strong>
             is equivalent to GHG emissions avoided by:</h4>"),
          valueBoxOutput(ns("waste")),
          valueBoxOutput(ns("trash")),
          valueBoxOutput(ns("lights"))
        )
      )
    )
  )
}

#' impact Server Functions
#'
#' @noRd
mod_impact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # home value box ----------------------------------------------------------

    output$home <- renderValueBox({
      valueBox("Home's energy use for one year",
        value = "63",
        icon = icon("home"),
        color = "green",
        width = NULL
      )
    })

    # gallons of gas value box ----------------------------------------------------------

    output$gallons <- renderValueBox({
      valueBox("Gallons of gas consumed",
        value = "56,262",
        icon = icon("gas-pump"),
        color = "yellow",
        width = NULL
      )
    })

    # smartphones value box ----------------------------------------------------------

    output$phones <- renderValueBox({
      valueBox("Number of smartphones charged",
        value = "60,821,323",
        icon = icon("mobile"),
        color = "blue",
        width = NULL
      )
    })

    # waste value box ----------------------------------------------------------

    output$waste <- renderValueBox({
      valueBox("Tons of waste recycled instead of landfilled ",
               value = 173,
               icon = icon("recycle"),
               color = "blue",
               width = NULL
      )
    })

    # trash bag value box ----------------------------------------------------------

    output$trash <- renderValueBox({
      valueBox("Trash bags of waste recycled instead of landfilled ",
               value = "63",
               icon = icon("trash"),
               color = "green",
               width = NULL
      )
    })

    # lights value box ----------------------------------------------------------

    output$lights <- renderValueBox({
      valueBox("Incandescent lamps switched to LEDs",
               value = "18,951",
               icon = icon("lightbulb"),
               color = "yellow",
               width = NULL
      )
    })
  })
}
## To be copied in the UI
# mod_impact_ui("impact_1")

## To be copied in the server
# mod_impacte_server("impact_1")
