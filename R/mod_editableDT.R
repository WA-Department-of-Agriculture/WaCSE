#' editableDT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#'
# TODO:   split UI for this tab into a different module
#         adding new row replaces previous row - need to fix

mod_editableDT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("county"),
          label = "County",
          choices = unique(comet_wa$county)
        ),
        selectizeInput(
          inputId = ns("class"),
          label = "Conservation Class",
          choices = unique(comet_wa$class)
        ),
        uiOutput(ns("practice")),
        uiOutput(ns("nutrient_practice")),
        uiOutput(ns("land_use")),
        uiOutput(ns("irrigation")),
        numericInput(
          inputId = ns("acres"),
          label = "Number of Acres",
          value = "1",
          min = 1
        ),
        actionButton(
          inputId = ns("add"),
          label = "Add"
        ),
        actionButton(
          inputId = ns("remove"),
          label = "Remove"
        )
      ),
      mainPanel(
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}

#' editableDT Server Functions
#'
#' @noRd
mod_editableDT_server <- function(id) {
  moduleServer(id = id, function(input, output, session) {
    ns <- session$ns


    # render UI inputs --------------------------------------------------------

    output$practice <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class) %>%
        select(practice) %>%
        arrange(practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("practice"),
        label = "Conservation Practice",
        choices = choices
      )
    })

    output$land_use <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(current_land_use) %>%
        arrange(current_land_use)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("land_use"),
        label = "Current Land Use",
        choices = choices
      )
    })

    output$irrigation <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice) %>%
        select(irrigation) %>%
        arrange(irrigation)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("irrigation"),
        label = "Irrigation Type",
        choices = choices
      )
    })

    output$nutrient_practice <- renderUI({
      req("Nutrient Management (CPS 590)" %in% input$practice)
      choices <- unique(comet_tags) %>%
        subset(practice %in% input$practice) %>%
        select(nutrient_practice) %>%
        arrange(nutrient_practice)

      choices <- as.character(pull(choices))

      selectizeInput(
        inputId = ns("nutrient_practice"),
        label = "Nutrient Management",
        choices = choices
      )
    })

    observeEvent(input$acres, {
      positive <- input$acres >= 1
      shinyFeedback::feedbackWarning("acres", !positive, "Please select at least one acre.")
      req(input$acres)
      return(input$acres)
    })

    # create reactive df ------------------------------------------------------

    # column names

    cols <- c(
      "County",
      "Conservation Class",
      "Conservation Practice",
      "Practice Implementation",
      "Acres",
      "Carbon Dioxide",
      "Nitrous Oxide",
      "Methane",
      "Total Greenhouse Gases"
    )

    # prepare data for table

    df <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(df) <- cols

    df <- reactiveVal(df)

    # filter to selected row

    filtered <- reactive({
      if (!("Nutrient Management (CPS 590)" %in% input$practice)) {
        filtered <- subset(
          comet_wa,
          county %in% input$county &
            class %in% input$class &
            practice %in% input$practice &
            current_land_use %in% input$land_use &
            irrigation %in% input$irrigation
        )
      } else {
        filtered <- subset(
          comet_wa,
          county %in% input$county &
            class %in% input$class &
            practice %in% input$practice &
            current_land_use %in% input$land_use &
            irrigation %in% input$irrigation &
            nutrient_practice %in% input$nutrient_practice
        )
      }

      filtered <- filtered %>%
        select(
          "county",
          "class",
          "practice",
          "implementation",
          "ghg_type",
          "mean"
        ) %>%
        tidyr::pivot_wider(
          names_from = ghg_type,
          values_from = mean
        )

      return(filtered)
    })

    # add new row to table

    observeEvent(input$add, {
      new_row <- data.frame(
        "County" = filtered()$county,
        "Conservation Class" = filtered()$class,
        "Conservation Practice" = filtered()$practice,
        "Practice Implementation" = filtered()$implementation,
        "Acres" = input$acres,
        "Carbon Dioxide" = input$acres * filtered()$co2,
        "Nitrous Oxide" = input$acres * filtered()$n2o,
        "Methane" = input$acres * filtered()$ch4,
        "Total Greenhouse Gases" = input$acres * filtered()$total.ghg.co2
      )

      new_row <- new_row %>%
        mutate(across(6:9, ~ replace(., is.na(.), "Not estimated")))

      rbind(df(new_row))

      return(df)
    })

    # render table ------------------------------------------------------------

    output$table <- DT::renderDataTable({
      DT::datatable(df())
    })
  })
}

## To be copied in the UI
# mod_editableDT_ui("editableDT_1")

## To be copied in the server
# mod_editableDT_server("editableDT_1")
