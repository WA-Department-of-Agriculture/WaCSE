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

  county_mlra <- comet_wa %>%
    select(county, mlra) %>%
    unique()
  cm_choices <- split(county_mlra$county, county_mlra$mlra)

  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "overflow: auto; max-height: 675px; position: relative;",
        div(
          id = ns("form"),
          selectizeInput(
            inputId = ns("county"),
            label = "County",
            choices = cm_choices,
            multiple = TRUE,
            selected = "Klickitat",
            options = list(
              plugins = list("remove_button")
            )
          ),
          selectizeInput(
            inputId = ns("class"),
            label = "Conservation Class",
            choices = unique(comet_tags$class),
            multiple = TRUE,
            selected = unique(comet_tags$class[1]),
            options = list(plugins = list("remove_button"))
          ),
          uiOutput(ns("practice")),
          uiOutput(ns("nutrient_practice")),
          uiOutput(ns("land_use")),
          uiOutput(ns("irrigation"))
        ),
        actionButton(ns("reset"), "Reset Selection")
      ),
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Table",
            br(),
            withSpinner(DT::DTOutput(ns("table"), width = "100%"))
          ),
          tabPanel(
            "Bar Graph",
            withSpinner(ggiraph::girafeOutput(ns("plot"), width = "100%"))
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

    # render UI filter elements and reactive df-----------------------------------------------

    output$practice <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class) %>%
        select(practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("practice"),
        label = "Conservation Practice",
        choices = choices,
        selected = choices[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$land_use <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(current_land_use)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("land_use"),
        label = "Current Land Use",
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$irrigation <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(irrigation)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("irrigation"),
        label = "Irrigation Type",
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$nutrient_practice <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice)
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice) %>%
        select(nutrient_practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("nutrient_practice"),
        label = "Nutrient Management",
        choices = choices,
        selected = c(choices["Not applicable"], choices[1:3]),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    filtered_df <- reactive({
      req(input$county, input$class, input$practice, input$land_use, input$irrigation)
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

    observeEvent(input$reset, {
      shinyjs::reset("form")
    })

    # render table ------------------------------------------------------------

    explore_table <- reactive({
      explore_table <- fct_table_filter(filtered_df()) %>%
        mutate(across(where(is.numeric), ~ replace(., is.na(.), "Not estimated"))) %>%
      fct_table(type = "explore")
      return(explore_table)
    })

    output$table <- DT::renderDT(explore_table())

    # render plot -------------------------------------------------------------

    explore_plot <- reactive({
        if (dplyr::n_distinct(filtered_df()$implementation) > 10 ||
            nrow(filtered_df()) > 40) {
          validate("The plot is too cluttered. Please remove some selections.")
        }
      explore_plot <- filtered_df() %>%
        filter(ghg_type == "total.ghg.co2") %>%
        fct_plot(type = "explore", error_bar = TRUE)
      return(explore_plot)
    })

    output$plot <- ggiraph::renderGirafe({explore_plot()})
  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
