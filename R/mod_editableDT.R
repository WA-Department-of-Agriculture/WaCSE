#' editableDT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinycssloaders
#' @import dplyr
#'
# TODO:   split UI for this tab into a different module
#         use proxy to update table rather than render
#         hide remove button from Summaries and Bar Graph tabs
#         edit df so SE is multiplied by acres
#         edit selectize inputs so user can type value


mod_editableDT_ui <- function(id) {
  ns <- NS(id)

  county_mlra <- comet_wa %>%
    select(county, mlra) %>%
    unique()
  cm_choices <- split(county_mlra$county, county_mlra$mlra)

  tagList(
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("county"),
          label = "County",
          choices = cm_choices,
        ),
        selectizeInput(
          inputId = ns("class"),
          label = "Conservation Class",
          choices = unique(comet_tags$class),
        ),
        selectizeInput(
          inputId = ns("practice"),
          label = "Conservation Practice",
          choices = unique(comet_tags$practice),
        ),
        selectizeInput(
          inputId = ns("implementation"),
          label = "Practice Implementation",
          choices = unique(comet_tags$implementation),
        ),
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
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Tables",
            h4("Full Table"),
            withSpinner(DT::DTOutput(ns("table"))),
            h4("Summary Table"),
            withSpinner(DT::DTOutput(ns("summary_county")))
          ),
          tabPanel(
            "Bar Graph", br(),
            withSpinner(ggiraph::girafeOutput(ns("plot"), width = "100%"))
          )
        )
      )
    )
  )
}

#' editableDT Server Functions
#'
#' @noRd
mod_editableDT_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # update or render UI inputs --------------------------------------------------------

    # update practice input

    observeEvent(input$class, {
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class) %>%
        select(practice) %>%
        arrange(practice)

      choices <- as.character(pull(choices))

      updateSelectizeInput(session, "practice", choices = choices)
    })

    # update implementation input

    observeEvent(
      eventExpr = {
        input$class
        input$practice
      },
      handlerExpr = {
        choices <- unique(comet_tags) %>%
          subset(
            class %in% input$class &
              practice %in% input$practice
          ) %>%
          select(implementation) %>%
          arrange(implementation)

        choices <- as.character(pull(choices))

        updateSelectizeInput(session, "implementation", choices = choices)
      }
    )

    # give warning if user selects acres <1

    observeEvent(input$acres, {
      positive <- input$acres >= 1
      shinyFeedback::feedbackWarning(
        "acres", !positive,
        "Please select at least one acre."
      )
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
            implementation %in% input$implementation
        )
      } else {
        filtered <- subset(
          comet_wa,
          county %in% input$county &
            class %in% input$class &
            practice %in% input$practice &
            implementation %in% input$implementation
        )
      }

      filtered <- fct_table_filter(filtered)

      return(filtered)
    })

    # add, edit, or delete rows ------------------------------------------------------

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
        mutate(across(where(is.numeric), ~ replace(., is.na(.), "Not estimated")))

      rv$df <- rbind(rv$df, tmp)

      rv$df <- unique(rv$df)
    })

    # remove row from table

    observeEvent(input$remove, {
      showModal(
        if (length(input$table_rows_selected) >= 1) {
          modalDialog(
            title = "Warning",
            paste(
              "Are you sure you want to delete",
              length(input$table_rows_selected), "row(s)?"
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm"), "Yes")
            ), easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Warning",
            paste("Please select the row(s) that you want to remove.
                  You can only select rows from the Table tab."),
            easyClose = TRUE
          )
        }
      )
    })

    # delete if user says okay

    observeEvent(input$confirm, {
      rv$df <- rv$df[-as.numeric(input$table_rows_selected), ]
      removeModal()
    })


    # summarize by county and practice ------------------------------------------

    # by county

    summary_county <- reactive({
      summary_county <- rv$df %>%
        mutate(
          Acres = as.numeric(Acres),
          Total.Greenhouse.Gases = as.numeric(Total.Greenhouse.Gases)
        ) %>%
        group_by(MLRA, County) %>%
        summarize(
          "# of Practice Implementations" = n_distinct(Practice.Implementation),
          "Total Acres" = sum(Acres),
          "Total Greenhouse Gases" = sum(Total.Greenhouse.Gases)
        ) %>%
        as.data.frame()
      return(summary_county)
    })

    # render tables ------------------------------------------------------------

    # full table

    output$table <- DT::renderDT({
      fct_table(rv$df, "estimate")
    })

    # summary tables

    output$summary_county <- DT::renderDT({
      fct_table(summary_county(), "summary_county")
    })

    # render plot -------------------------------------------------------------

    filtered_plot <- reactive({
      rv$df %>%
        select(
          mlra = MLRA,
          county = County,
          abbr = Practice.Implementation,
          implementation = Practice.Implementation,
          acres = Acres,
          mean = Total.Greenhouse.Gases
        ) %>%
        mutate(mean = as.numeric(mean))
    })

    output$plot <- ggiraph::renderGirafe({
      req(filtered_plot())
      if (dplyr::n_distinct(filtered_plot()$implementation) > 10 ||
        nrow(filtered_plot()) > 40) {
        validate("The plot is too cluttered. Please remove some selections.")
      }
      fct_plot(filtered_plot(), type = "estimate", error_bar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_editableDT_ui("editableDT_1")

## To be copied in the server
# mod_editableDT_server("editableDT_1")
