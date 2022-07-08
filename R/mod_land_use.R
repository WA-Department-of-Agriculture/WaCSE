#' land_use UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom shinydashboard box valueBoxOutput renderValueBox valueBox

mod_land_use_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
        box(
          title = strong("Agricultural Acres by County"),
          width = 3, status = "primary", collapsible = TRUE, solidHeader = TRUE,
          virtualSelectInput(
            inputId = ns("county"),
            label = strong("County"),
            choices = unique(comet_wa$county),
            selected = unique(comet_wa$county),
            multiple = TRUE,
            search = TRUE,
            position = "bottom",
            optionsCount = 5
          ),
          valueBoxOutput(ns("cropland"), width = 12),
          valueBoxOutput(ns("pasture"), width = 12),
          valueBoxOutput(ns("other"), width = 12)
      ),
      column(
        width = 9,
        box(
          title = strong("Maps"),
          width = NULL, status = "primary", collapsible = TRUE, solidHeader = TRUE,
        tabsetPanel(
          type = "pills",
          tabPanel("Crop Group Map", icon = icon("carrot"), br(),
                   htmlOutput(ns("crop_map"))),
          tabPanel("Major Land Resource Area Map", icon = icon("mountain"), br(),
                   htmlOutput(ns("mlra_map")))
        )
        )
      )
    )
  )
}

#' land_use Server Functions
#'
#' @noRd
mod_land_use_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # cropland indicator

    value_cropland <- reactive({
      value_cropland <- fct_sumAcres(county = input$county, type = "cropland") %>%
        prettyNum(big.mark = ",")
    })

    output$cropland <- renderValueBox({
      valueBox(subtitle = HTML("<b>Cropland Acres</b> <br>
                                All crop groups except developed,
                                other, pasture, shellfish, and turfgrass."),
              value = paste(value_cropland(), "Ac"),
              icon = icon("apple"),
              color = "red",
              width = 12
      )
    })

    # pasture indicator

    # "Includes only pasture.</center>"

    value_pasture <- reactive({
      value_cropland <- fct_sumAcres(county = input$county, type = "pasture") %>%
        prettyNum(big.mark = ",")
    })

    output$pasture <- renderValueBox({
      valueBox(subtitle = HTML("<b>Pasture Acres</b> <br>
                               Includes pasture only."),
               value = paste(value_pasture(), "Ac"),
               icon = icon("horse"),
               color = "yellow",
               width = 12
      )
    })

    # other indicator

    value_other <- reactive({
      value_cropland <- fct_sumAcres(county = input$county, type = "other") %>%
        prettyNum(big.mark = ",")
    })

    output$other <- renderValueBox({
      valueBox(subtitle = HTML("<b>'Other' Ag Acres</b> <br>
                               Includes conservation, fallow,
                               wildlife feed, research, unknown, and alkali bee bed"),
               value = paste(value_other(), "Ac"),
               icon = icon("leaf"),
               color = "green",
               width = 12
      )
    })

    # ArcGIS crops iframe

    output$crop_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=1e0f1a416e1b4f9ea0dfb10b82e53da7",
        height = "500px",
        width = "100%"
      )
    })

    # ArcGIS MLRA iframe

    output$mlra_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=47fe1baea32e46248c3fb8e6958a1c30",
        height = "500px",
        width = "100%"
      )
    })
  })
}

## To be copied in the UI
# mod_land_use_ui("land_use_1")

## To be copied in the server
# mod_land_use_server("land_use_1")
