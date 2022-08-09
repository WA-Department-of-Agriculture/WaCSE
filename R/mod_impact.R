#'  impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        includeMarkdown(normalizePath("inst/app/www/rmd/impactAbout.md")),
        numericInput(
          inputId = ns("ghg_input"),
          label = strong("Enter your Total GHG Reduction from the
                         Calculate your estimate tab:"),
          value = NULL,
          min = 0,
          width = 500,
        ),
        actionButton(
          inputId = ns("convert"),
          label = "Convert",
          class = "btn-success",
          ),
        hr(),
        fluidRow(
          h5(strong(textOutput(outputId = ns("ghgCO2eq")))),
          shinydashboard::valueBoxOutput(outputId = ns("home")),
          shinydashboard::valueBoxOutput(outputId = ns("gallons")),
          shinydashboard::valueBoxOutput(outputId = ns("phones"))
        ),
        fluidRow(
          h5(strong(textOutput(outputId = ns("ghgAvoided")))),
          shinydashboard::valueBoxOutput(outputId = ns("waste")),
          shinydashboard::valueBoxOutput(outputId = ns("trash")),
          shinydashboard::valueBoxOutput(outputId = ns("lights"))
        ),
        fluidRow(
          h5(strong(textOutput(outputId = ns("Cseq")))),
          shinydashboard::valueBoxOutput(outputId = ns("seedling")),
          shinydashboard::valueBoxOutput(outputId = ns("forest")),
          shinydashboard::valueBoxOutput(outputId = ns("conversion"))
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

    # update ghg emissions estimate from the input upon click of convert button

    ghg_input <- eventReactive(input$convert, {
      ifelse(input$ghg_input %in% c("", NULL, NA), 0, input$ghg_input)
    })

    # CO2 emissions from row --------------------------------------------------

    output$ghgCO2eq <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to
            CO2 emissions from:")
    })

    # home value box ----------------------------------------------------------

    output$home <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Homes' electricity use for one year",
        value = fct_ghgEq(ghg_input(), "home"),
        icon = icon("house"),
        color = "yellow",
        width = NULL
      )
    })

    # gallons of gas value box ------------------------------------------------

    output$gallons <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Gallons of gas consumed",
        value = fct_ghgEq(ghg_input(), "gas"),
        icon = icon("gas-pump"),
        color = "yellow",
        width = NULL
      )
    })

    # smartphones value box ---------------------------------------------------

    output$phones <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Number of smartphones charged",
        value = fct_ghgEq(ghg_input(), "smartphone"),
        icon = icon("mobile"),
        color = "yellow",
        width = NULL
      )
    })

    # emissions avoided by row -------------------------------------------------

    output$ghgAvoided <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to
            GHG emissions avoided by:")
    })

    # waste value box ----------------------------------------------------------

    output$waste <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Tons of waste recycled instead of landfilled",
        value = fct_ghgEq(ghg_input(), "waste"),
        icon = icon("recycle"),
        color = "blue",
        width = NULL
      )
    })

    # trash bag value box ------------------------------------------------------

    output$trash <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Trash bags of waste recycled instead of landfilled",
        value = fct_ghgEq(ghg_input(), "trash"),
        icon = icon("trash"),
        color = "blue",
        width = NULL
      )
    })

    # lights value box ---------------------------------------------------------

    output$lights <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Incandescent lamps switched to LEDs",
        value = fct_ghgEq(ghg_input(), "light"),
        icon = icon("lightbulb"),
        color = "blue",
        width = NULL
      )
    })

    # carbon sequestered row --------------------------------------------------

    output$Cseq <- renderText({
      paste(ghg_input(), "metric tonnes of CO2eq is equivalent to
            carbon sequestered by:")
    })

    # seedling value box -------------------------------------------------------

    output$seedling <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Tree seedlings grown for 10 years",
        value = fct_ghgEq(ghg_input(), "seedling"),
        icon = icon("seedling"),
        color = "green",
        width = NULL
      )
    })

    # forest value box ---------------------------------------------------------

    output$forest <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Acres of US forests in one year",
        value = fct_ghgEq(ghg_input(), "forest"),
        icon = icon("tree"),
        color = "green",
        width = NULL
      )
    })

    # conversion value box -----------------------------------------------------

    output$conversion <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Acres of US forests preserved from
        conversion to cropland in one year",
        value = fct_ghgEq(ghg_input(), "conversion"),
        icon = icon("wheat-awn"),
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
