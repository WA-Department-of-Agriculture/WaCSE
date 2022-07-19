#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom shinydashboard box

mod_explore_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        4,
        box(
          title = strong("Definitions"),
          width = NULL,
          status = "primary",
          collapsible = TRUE,
          solidHeader = TRUE,
          includeMarkdown(
            normalizePath("inst/app/www/rmd/exploreDefinitions.md"))
        ),
        box(
          title = tagList(
            span(strong("Filter the data")),
            span(fct_helpBtn(id = ns("filterHelp")))
          ),
          width = NULL,
          status = "primary",
          collapsible = TRUE,
          solidHeader = TRUE,
          virtualSelectInput(
            inputId = ns("county"),
            label = strong("Step 1. County"),
            choices = sort(unique(comet_wa$county)),
            multiple = TRUE,
            search = TRUE,
            position = "bottom",
            optionsCount = 5,
            autoSelectFirstOption = TRUE,
            showValueAsTags = TRUE
          ),
          virtualSelectInput(
            inputId = ns("class"),
            label = strong("Step 2. Conservation Class"),
            choices = sort(unique(comet_tags$class)),
            multiple = TRUE,
            position = "bottom",
            optionsCount = 5,
            autoSelectFirstOption = TRUE,
            showValueAsTags = TRUE
          ),
          uiOutput(outputId = ns("practice")),
          uiOutput(outputId = ns("land_use")),
          uiOutput(outputId = ns("irrigation")),
          uiOutput(outputId = ns("nutrient_practice"))
        )
      ),
      column(
        8,
        box(
          title = tagList(
            span(strong("Explore the data")),
            span(fct_helpBtn(ns("exploreHelp")))
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
              DT::DTOutput(ns("table"))
            ),
            tabPanel(
              "Bar Graph",
              icon = icon("chart-bar"),
              ggiraph::girafeOutput(ns("plot"))
            ),
            tabPanel(
              "MLRA Map",
              icon = icon("map"),
              br(),
              includeMarkdown(normalizePath("inst/app/www/rmd/mlra.md")),
              htmlOutput(ns("mlra_map"))
            )
          )
        )
      )
    )
  )
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

    # render UI filter elements -----------------------------------------------

    output$practice <- renderUI({
      choices <- unique(comet_wa) %>%
        subset(county %in% input$county &
          class %in% input$class) %>%
        select(practice)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("practice"),
        label = strong("Step 3. Conservation Practice"),
        choices = sort(unique(choices)),
        multiple = TRUE,
        search = TRUE,
        position = "bottom",
        optionsCount = 5,
        autoSelectFirstOption = TRUE,
        showValueAsTags = TRUE
      )
    })

    output$land_use <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(current_land_use)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("land_use"),
        label = strong("Step 4. Current Land Use"),
        choices = sort(unique(choices)),
        multiple = TRUE,
        selected = choices,
        position = "bottom",
        optionsCount = 5,
        showValueAsTags = TRUE
      )
    })

    output$irrigation <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(irrigation)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("irrigation"),
        label = strong("Step 5. Irrigation Type"),
        choices = sort(unique(choices)),
        selected = choices,
        multiple = TRUE,
        position = "bottom",
        optionsCount = 5,
        showValueAsTags = TRUE
      )
    })

    output$nutrient_practice <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice)
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice) %>%
        select(nutrient_practice)

      choices <- as.character(pull(choices))

      tagList(
        virtualSelectInput(
          inputId = ns("nutrient_practice"),
          label = strong("Step 6. Nutrient Management*"),
          choices = sort(unique(choices)),
          selected = c(choices["Not Applicable"], choices[1:3]),
          multiple = TRUE,
          position = "bottom",
          optionsCount = 5,
          search = TRUE,
          showValueAsTags = TRUE
        ),
        p("*",
          em("If you selected multiple practices in Step 3,
              select 'Not Applicable' in Step 6 to include all practices."))
      )
    })

    # render df for proxy -----------------------------------------------------

    df <- data.frame(
      "MLRA" = character(),
      "County" = character(),
      "Conservation Class" = character(),
      "Conservation Practice" = character(),
      "Practice Implementation" = character(),
      "Carbon Dioxide" = numeric(),
      "Nitrous Oxide" = numeric(),
      "Methane" = numeric(),
      "Total Greenhouse Gases" = numeric()
    )

    # render reactive df ------------------------------------------------------

    filtered_df <- reactive({
      req(input$county,
          input$class,
          input$practice,
          input$land_use,
          input$irrigation)
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
        filter(ghg_type == "total.ghg.co2") %>%
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
