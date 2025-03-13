#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_explore_ui <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      4,

      # definitions box ---------------------------------------------------------

      shinydashboard::box(
        title = strong("Definitions"),
        width = NULL,
        status = "primary",
        collapsible = TRUE,
        solidHeader = TRUE,
        includeMarkdown(normalizePath(
          "inst/app/www/rmd/exploreDefinitions.md"
        ))
      ),

      # filter box --------------------------------------------------------------

      shinydashboard::box(
        title = tagList(
          span(strong("Filter the data")),
          span(fct_helpBtn(
            id = ns("filterHelp")
          ))
        ),
        width = NULL,
        status = "primary",
        collapsible = TRUE,
        solidHeader = TRUE,
        shinyWidgets::virtualSelectInput(
          inputId = ns("county"),
          label = strong("Step 1. County"),
          placeholder = "Select counties",
          choices = sort(unique(comet_wa$county)),
          multiple = TRUE,
          search = TRUE,
          position = "bottom",
          showValueAsTags = TRUE,
          optionsCount = 5
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("class"),
          label = span(
            strong("Step 2. Conservation Class"),
            br(),
            helpText(
              "Select NRCS categories that describe the practices
                       you are interested in."
            )
          ),
          placeholder = "Select conservation classes",
          choices = sort(unique(comet_tags$class)),
          multiple = TRUE,
          position = "bottom",
          showValueAsTags = TRUE
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("practice"),
          label = span(
            strong("Step 3. Conservation Practice"),
            br(),
            helpText(
              "Select NRCS conservation practice standards
                       (CPS) you are interested in."
            )
          ),
          placeholder = "Select conservation practices",
          choices = NULL,
          multiple = TRUE,
          search = TRUE,
          position = "bottom",
          showValueAsTags = TRUE,
          optionsCount = 5
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("land_use"),
          label = span(
            strong("Step 4. Current Land Use"),
            br(),
            helpText("Select how the land currently is used.")
          ),
          placeholder = "Select land uses",
          choices = NULL,
          multiple = TRUE,
          position = "bottom",
          showValueAsTags = TRUE
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("irrigation"),
          label = span(
            strong("Step 5. Irrigation Type"),
            br(),
            helpText("Select how the current system is irrigated.")
          ),
          placeholder = "Select irrigation types",
          choices = NULL,
          multiple = TRUE,
          position = "bottom",
          showValueAsTags = TRUE
        ),
        uiOutput(outputId = ns("nutrient_practice"))
      )
    ),
    column(
      8,

      # explore the data box ----------------------------------------------------

      shinydashboard::box(
        title = tagList(
          span(strong("Explore the data")),
          span(fct_helpBtn(
            ns("exploreHelp")
          ))
        ),
        width = NULL,
        status = "primary",
        collapsible = TRUE,
        solidHeader = TRUE,
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Table",
            icon = icon("table"),
            br(),
            includeMarkdown(normalizePath("inst/app/www/rmd/aboveTableText.md")),
            shinycssloaders::withSpinner(DT::DTOutput(ns("table")))
          ),
          tabPanel(
            "Bar Graph",
            icon = icon("chart-bar"),
            shinycssloaders::withSpinner(ggiraph::girafeOutput(ns("plot")))
          ),
          tabPanel(
            "MLRA Map",
            icon = icon("map"),
            br(),
            includeMarkdown(normalizePath("inst/app/www/rmd/exploreMLRA.md")),
            htmlOutput(ns("mlra_map"))
          )
        )
      )
    )
  ))
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # help modals -------------------------------------------------------------

    # modal for filter help

    observeEvent(input$filterHelp, {
      fct_helpModal("exploreFilter")
    })

    # modal for explore help

    observeEvent(input$exploreHelp, {
      fct_helpModal("exploreExplore")
    })

    # update select input choices  --------------------------------------------

    # practice input

    observeEvent(
      eventExpr = {
        input$county
        input$class
      },
      handlerExpr = {
        choices <-
          unique(comet_wa$practice[comet_wa$county %in% input$county &
            comet_wa$class %in% input$class])

        shinyWidgets::updateVirtualSelect(
          inputId = "practice",
          choices = sort(choices),
          selected = input$practice
        )
      }
    )

    # land use input

    observeEvent(
      eventExpr = {
        input$class
        input$practice
      },
      handlerExpr = {
        choices <- unique(comet_wa$current_land_use[comet_wa$class %in% input$class &
          comet_wa$practice %in% input$practice])

        shinyWidgets::updateVirtualSelect(
          inputId = "land_use",
          choices = sort(choices),
          selected = input$land_use
        )
      }
    )

    # irrigation input

    observeEvent(
      eventExpr = {
        input$class
        input$practice
      },
      handlerExpr = {
        choices <- unique(comet_wa$irrigation[comet_wa$class %in% input$class &
          comet_wa$practice %in% input$practice])

        shinyWidgets::updateVirtualSelect(
          inputId = "irrigation",
          choices = sort(choices),
          selected = input$irrigation
        )
      }
    )

    # nutrient practice

    output$nutrient_practice <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice)
      choices <- unique(comet_wa$nutrient_practice[comet_wa$class %in% input$class &
        comet_wa$practice %in% input$practice])

      tagList(
        shinyWidgets::virtualSelectInput(
          inputId = ns("nutrient_practice"),
          label = span(
            strong("Step 6. Nutrient Management"),
            br(),
            helpText(
              "Select if you want to reduce your
                     application rate or change your nutrient source."
            ),
            br(),
            helpText(
              em(
                "If you selected multiple practices in Step 3,
              'Not Applicable' must be selected in Step 6 to include
              all selected practices."
              )
            )
          ),
          placeholder = "Select nutrient management",
          choices = sort(unique(choices)),
          selected = input$nutrient_practice,
          multiple = TRUE,
          position = "bottom",
          search = TRUE,
          showValueAsTags = TRUE
        )
      )
    })

    # render df for proxy -----------------------------------------------------

    df <- data.frame(
      "mlra" = character(),
      "county" = character(),
      "class" = character(),
      "practice" = character(),
      "implementation" = character(),
      "co2" = numeric(),
      "n2o" = numeric(),
      "ch4" = numeric(),
      "total_ghg_co2" = numeric()
    )

    # render reactive df ------------------------------------------------------

    filtered_df <- reactive({
      req(
        input$county,
        input$class,
        input$practice,
        input$land_use,
        input$irrigation
      )
      if (!("Nutrient Management (CPS 590)" %in% input$practice)) {
        filtered_df <- subset(
          comet_wa,
          county %in% input$county &
            class %in% input$class &
            practice %in% input$practice &
            current_land_use %in% input$land_use &
            irrigation %in% input$irrigation
        )
        return(filtered_df)
      } else {
        req(input$nutrient_practice)
        filtered_df <- subset(
          comet_wa,
          county %in% input$county &
            class %in% input$class &
            practice %in% input$practice &
            current_land_use %in% input$land_use &
            irrigation %in% input$irrigation &
            nutrient_practice %in% input$nutrient_practice
        )
        return(filtered_df)
      }
    })

    # render table ------------------------------------------------------------

    explore_table <- reactive({
      explore_table <- fct_tableFilter(filtered_df())
      return(explore_table)
    })

    output$table <- DT::renderDT({
      fct_table(df, type = "explore")
    })

    proxy <- DT::dataTableProxy("table")

    observe({
      DT::replaceData(proxy, explore_table(), rownames = FALSE)
    })

    # render plot -------------------------------------------------------------

    explore_plot <- reactive({
      if (dplyr::n_distinct(filtered_df()$implementation) > 12 ||
        nrow(filtered_df()) > 100) {
        validate("The graph is too cluttered. Please remove some selections.")
      }
      explore_plot <- filtered_df() %>%
        filter(ghg_type == "total_ghg_co2") %>%
        fct_plot(type = "explore", error_bar = TRUE)
      return(explore_plot)
    })

    output$plot <- ggiraph::renderGirafe({
      explore_plot()
    })

    # ArcGIS MLRA iframe

    output$mlra_map <- renderUI({
      tags$iframe(
        src = "https://nras.maps.arcgis.com/apps/instant/basic/index.html?appid=4233536b08044da7a9bc32c7040418be",
        height = "500px",
        width = "100%"
      )
    })
  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
