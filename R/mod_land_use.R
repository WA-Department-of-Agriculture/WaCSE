#' land_use UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboard

# TODO: add tooptip info boxes for valueBox to list what each category includes
#       create select/deselect all buttons for county

mod_land_use_ui <- function(id) {
  ns <- NS(id)

  county_mlra <- comet_wa %>%
    select(county, mlra) %>%
    unique()
  cm_choices <- split(county_mlra$county, county_mlra$mlra)

  tagList(fluidRow(
    column(
      3,
      selectizeInput(
        inputId = ns("county"),
        label = "County",
        choices = cm_choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"),
                       placeholder = "Pick your counties")
      )
    ),
    column(3, htmlOutput(ns("cropland"))),
    column(3, htmlOutput(ns("pasture"))),
    column(3, htmlOutput(ns("other")))
  ), hr(),
  fluidRow(htmlOutput(ns("map"))))
}

#' land_use Server Functions
#'
#' @noRd
mod_land_use_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cropland <- renderText({
      paste(
        "<b><center>Cropland Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "cropland"),
        "<br>",
        "Excludes developed, other, pasture, shellfish, and turfgrass.</center>"
      )
    })

    output$pasture <- renderText({
      paste(
        "<b><center>Pasture Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "pasture"),
        "<br>",
        "Includes only pasture.</center>"
      )
    })


    output$other <- renderText({
      paste(
        "<b><center>Other Ag Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "other"),
        "<br>",
        "Includes only conservation, fallow, research, unknown, and wildlife feed.</center>"
      )
    })

    output$map <- renderUI({
      tags$iframe(src = "https://arcg.is/18iDi1",
                  width = "100%",
                  height = "600px")
    })
  })
}

## To be copied in the UI
# mod_land_use_ui("land_use_1")

## To be copied in the server
# mod_land_use_server("land_use_1")
