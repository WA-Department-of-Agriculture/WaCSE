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

# TODO: get total GHG emissions from estimate tab as a reactive then use updateNumericInput

mod_impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12, status = "primary",
        includeMarkdown(normalizePath("inst/app/www/rmd/impactAbout.md")),
        numericInput(ns("ghg_input"),
          label = strong("Enter CO2eq emissions in metric tonnes:"),
          value = 50,
          width = 300
        ),
        fluidRow(
          h5(strong(textOutput(ns("ghgCO2eq")))),
          valueBoxOutput(ns("home")),
          valueBoxOutput(ns("gallons")),
          valueBoxOutput(ns("phones"))
        ),
        fluidRow(
          h5(strong(textOutput(ns("ghgAvoided")))),
          valueBoxOutput(ns("waste")),
          valueBoxOutput(ns("trash")),
          valueBoxOutput(ns("lights"))
        ),
        fluidRow(
          h5(strong(textOutput(ns("Cseq")))),
          valueBoxOutput(ns("seedling")),
          valueBoxOutput(ns("forest")),
          valueBoxOutput(ns("conversion"))
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

    # get ghg emissions estimate from the input

    ghg_input <- reactive({
      req(input$ghg_input)
      input$ghg_input
    })

    # CO2 emissions from row --------------------------------------------------

    output$ghgCO2eq <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to CO2 emissions from:")
    })

    # home value box ----------------------------------------------------------

    output$home <- renderValueBox({
      valueBox("Home's electricity use for one year",
        value = fct_ghgEq(ghg_input(), "home"),
        icon = icon("home"),
        color = "yellow",
        width = NULL
      )
    })

    # gallons of gas value box ----------------------------------------------------------

    output$gallons <- renderValueBox({
      valueBox("Gallons of gas consumed",
        value = fct_ghgEq(ghg_input(), "gas"),
        icon = icon("gas-pump"),
        color = "yellow",
        width = NULL
      )
    })

    # smartphones value box ----------------------------------------------------------

    output$phones <- renderValueBox({
      valueBox("Number of smartphones charged",
        value = fct_ghgEq(ghg_input(), "smartphone"),
        icon = icon("mobile"),
        color = "yellow",
        width = NULL
      )
    })


    # emissions avoided by row ----------------------------------------------------

    output$ghgAvoided <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to GHG emissions avoided by:")
    })

    # waste value box ----------------------------------------------------------

    output$waste <- renderValueBox({
      valueBox("Tons of waste recycled instead of landfilled",
        value = fct_ghgEq(ghg_input(), "waste"),
        icon = icon("recycle"),
        color = "blue",
        width = NULL
      )
    })

    # trash bag value box ----------------------------------------------------------

    output$trash <- renderValueBox({
      valueBox("Trash bags of waste recycled instead of landfilled",
        value = fct_ghgEq(ghg_input(), "trash"),
        icon = icon("trash"),
        color = "blue",
        width = NULL
      )
    })

    # lights value box ----------------------------------------------------------

    output$lights <- renderValueBox({
      valueBox("Incandescent lamps switched to LEDs",
        value = fct_ghgEq(ghg_input(), "light"),
        icon = icon("lightbulb"),
        color = "blue",
        width = NULL
      )
    })


    # carbon sequestered row --------------------------------------------------

    output$Cseq <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to carbon sequestered by:")
    })

    # seedling value box ----------------------------------------------------------

    output$seedling <- renderValueBox({
      valueBox("Tree seedlings grown for 10 years",
        value = fct_ghgEq(ghg_input(), "seedling"),
        icon = icon("seedling"),
        color = "green",
        width = NULL
      )
    })

    # forest value box ----------------------------------------------------------

    output$forest <- renderValueBox({
      valueBox("Acres of US forests in one year",
        value = fct_ghgEq(ghg_input(), "forest"),
        icon = icon("tree"),
        color = "green",
        width = NULL
      )
    })

    # conversion value box ----------------------------------------------------------

    output$conversion <- renderValueBox({
      valueBox("Acres of US forests preserved from conversion to cropland in one year",
        value = fct_ghgEq(ghg_input(), "conversion"),
        icon = icon("apple"),
        color = "green",
        width = NULL
      )
    })
  })
}
## To be copied in the UI
# mod_impact_ui("impact_1")

## To be copied in the server
# mod_impacte_server("impact_1")
