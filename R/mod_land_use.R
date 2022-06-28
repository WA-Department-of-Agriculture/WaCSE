#' land_use UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyBS

# TODO: add tooptip info boxes for valueBox to list what each category includes

mod_land_use_ui <- function(id) {
  ns <- NS(id)

counties <- as.character(unique(comet_wa$county))
choices <- c("All Counties", counties)

  tagList(
    includeCSS("inst/app/www/css/style.css"),
    tabsetPanel(
      type = "pills",
      tabPanel("Total Acres by County", br(),
          fluidRow(column(
            3, selectizeInput(
              inputId = ns("county"),
              label = "County",
              choices = choices,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Pick your counties"
              )
            ),
            actionButton(ns("reset"), "Reset")
          ),
          column(3, wellPanel(style = "height:100%", htmlOutput(ns("cropland")))),
          column(3, wellPanel(style = "height:100%", htmlOutput(ns("pasture")))),
          column(3, wellPanel(style = "height:100%", htmlOutput(ns("other")))),
               )
          ),
      tabPanel("Crop Group Map", br(), htmlOutput(ns("crop_map"))),
      tabPanel("Major Land Resource Area Map", br(), htmlOutput(ns("mlra_map")))
    )
  )
}

#' land_use Server Functions
#'
#' @noRd
mod_land_use_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    counties <- as.character(unique(comet_wa$county))
    choices <- c("All Counties", counties)

    # if user selects "All Counties", input will update to select all choices besides "All Counties"

    observe({
      if ("All Counties" %in% input$county) {
        selected_choices <- setdiff(choices, "All Counties")
        updateSelectizeInput(session, "county", selected = selected_choices)
      }
    })

    # reset filter on Reset button click

    observeEvent(input$reset, {
      shinyjs::reset("county")
    })

    # cropland indicator

    output$cropland <- renderText({
      paste(
        "<b><center>Cropland Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "cropland"),
        "<br>",
        "Excludes developed, other, pasture, shellfish, and turfgrass.</center>"
      )
    })

    # pasture indicator

    output$pasture <- renderText({
      paste(
        "<b><center>Pasture Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "pasture"),
        "<br>",
        "Includes only pasture.</center>"
      )
    })

    # other indicator

    output$other <- renderText({
      paste(
        "<b><center>Other Ag Acres</b>",
        "<br>",
        fct_sumAcres(county = input$county, type = "other"),
        "<br>",
        "Includes only conservation, fallow, research, unknown, and wildlife feed.</center>"
      )
    })

    # ArcGIS crops iframe

    output$crop_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=1e0f1a416e1b4f9ea0dfb10b82e53da7",
        height = "600px",
        width = "100%"
      )
    })

    # ArcGIS MLRA iframe

    output$mlra_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=47fe1baea32e46248c3fb8e6958a1c30",
        height = "600px",
        width = "100%"
      )
    })
  })
}

## To be copied in the UI
# mod_land_use_ui("land_use_1")

## To be copied in the server
# mod_land_use_server("land_use_1")
