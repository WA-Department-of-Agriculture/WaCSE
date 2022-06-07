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
#         use proxy to update table rather than render
#         add totals row or indicators to sum emission reductions


mod_editableDT_ui <- function(id) {
  ns <- NS(id)

  county_mlra <- comet_wa %>% select(county, mlra) %>% unique()
  cm_choices = split(county_mlra$county, county_mlra$mlra)

  tagList(
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("county"),
          label = "County",
          choices = cm_choices
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
        width = 9,
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

    # prepare data for table

    rv <- reactiveValues()

    rv$df <- data.frame(
      "MLRA" = character(),
      "County" = character(),
      "Conservation Class" = character(),
      "Conservation Practice" = character(),
      "Practice Implementation" = character(),
      "Acres" = numeric(),
      "Carbon Dioxide" = numeric(),
      "Nitrous Oxide" = numeric(),
      "Methane" = numeric(),
      "Total Greenhouse Gases" = numeric()
    )

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

      filtered <- fct_table_filter(filtered)

      return(filtered)
    })


# add or delete rows ------------------------------------------------------

    # add new row to table

    observeEvent(input$add, {
      req(
        input$county,
        input$class,
        input$practice,
        input$acres,
        filtered()$implementation,
        filtered()$co2
      )

      tmp <- data.frame(
        "MLRA" = filtered()$mlra,
        "County" = filtered()$county,
        "Conservation Class" = filtered()$class,
        "Conservation Practice" = filtered()$practice,
        "Practice Implementation" = filtered()$implementation,
        "Acres" = input$acres,
        "Carbon Dioxide" = input$acres * filtered()$co2,
        "Nitrous Oxide" = input$acres * filtered()$n2o,
        "Methane" = input$acres * filtered()$ch4,
        "Total Greenhouse Gases" = input$acres * filtered()$total.ghg.co2
      ) %>%
        mutate(across(6:9, ~ replace(., is.na(.), "Not estimated")))

      rv$df <- rbind(rv$df, tmp)

      rv$df <- unique(rv$df)

    })

    # remove row from table

    observeEvent(input$remove, {
      showModal(
        if (length(input$table_rows_selected) >= 1) {
          modalDialog(
            title = "Warning",
            paste("Are you sure you want to delete", length(input$table_rows_selected), "row(s)?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm"), "Yes")
            ), easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Warning",
            paste("Please select the row(s) that you want to delete."), easyClose = TRUE
          )
        }
      )
    })

    # delete if user says okay
    observeEvent(input$confirm, {
      rv$df <- rv$df[-as.numeric(input$table_rows_selected), ]
      removeModal()
    })

    # render table ------------------------------------------------------------

    output$table <- DT::renderDataTable({
      fct_table(rv$df, "estimate")
    })
  })
}

## To be copied in the UI
# mod_editableDT_ui("editableDT_1")

## To be copied in the server
# mod_editableDT_server("editableDT_1")
