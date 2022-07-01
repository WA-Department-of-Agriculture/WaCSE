#' estimate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @importFrom shinyWidgets virtualSelectInput updateVirtualSelect actionBttn
#' @importFrom shinydashboard box infoBoxOutput renderInfoBox infoBox
#'
# TODO:   split UI for this tab into a different module
#         hide remove button from Summaries and Bar Graph tabs

mod_estimate_ui <- function(id) {
  ns <- NS(id)

  county_mlra <- comet_wa %>%
    select(county, mlra) %>%
    unique()
  cm_choices <- split(county_mlra$county, county_mlra$mlra)

  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = strong("Add Practices by County and Acres"),
          width = NULL, status = "primary", collapsible = TRUE, solidHeader = TRUE,
          virtualSelectInput(
            inputId = ns("county"),
            label = strong("1. County"),
            choices = cm_choices,
            selected = "Klickitat",
            multiple = FALSE,
            position = "bottom",
            optionsCount = 5,
            search = TRUE
          ),
          virtualSelectInput(
            inputId = ns("class"),
            label = strong("2. Conservation Class"),
            choices = unique(comet_tags$class),
            multiple = FALSE,
            position = "bottom",
            optionsCount = 5
          ),
          uiOutput(ns("practice")),
          uiOutput(ns("land_use")),
          uiOutput(ns("irrigation")),
          uiOutput(ns("implementation")),
          numericInput(
            inputId = ns("acres"),
            label = strong("7. Number of Acres"),
            value = "1",
            min = 1
          ),
          actionBttn(
            inputId = ns("add"),
            label = "Add",
            style = "simple",
            color = "success"
          ),
          actionBttn(
            inputId = ns("remove"),
            label = "Remove",
            style = "simple",
            color = "danger"
          )
        )
      ), column(
        width = 8,
        fluidRow(
          box(
            title = strong("View your Estimate"),
            width = NULL, status = "primary", collapsible = TRUE, solidHeader = TRUE,
            tabsetPanel(
              type = "pills",
              tabPanel(
                "Table",
                icon = icon("table"), br(),
                DT::DTOutput(ns("table"))
              ),
              tabPanel("Bar Graph", icon = icon("chart-bar"), ggiraph::girafeOutput(ns("plot")))
            )
          )
        ),
        fluidRow(div(
          class = "infoBoxes",
          infoBoxOutput(ns("total_acres")),
          infoBoxOutput(ns("total_ghg"))
        )),
        box(
          title = strong("Summary by County"),
          width = NULL, status = "primary", collapsible = TRUE, solidHeader = TRUE,
          DT::DTOutput(ns("summary"))
        )
      )
    )
  )
}

#' estimate Server Functions
#'
#' @noRd
mod_estimate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # update or render UI inputs --------------------------------------------------------

    # render practice input

    output$practice <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class) %>%
        select(practice)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("practice"),
        label = strong("3. Conservation Practice"),
        choices = sort(unique(choices)),
        multiple = FALSE,
        position = "bottom",
        optionsCount = 5,
        search = TRUE
      )
    })

    # render land use input

    output$land_use <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(current_land_use)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("land_use"),
        label = strong("4. Current Land Use"),
        choices = sort(unique(choices)),
        multiple = FALSE,
        position = "bottom",
        optionsCount = 5
      )
    })

    # render irrigation input

    output$irrigation <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice) %>%
        select(irrigation)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("irrigation"),
        label = strong("5. Irrigation Type"),
        choices = sort(unique(choices)),
        multiple = FALSE,
        position = "bottom",
        optionsCount = 5
      )
    })

    # render implementation input

    output$implementation <- renderUI({
      choices <- unique(comet_tags) %>%
        subset(class %in% input$class &
          practice %in% input$practice &
          current_land_use %in% input$land_use &
          irrigation %in% input$irrigation) %>%
        select(implementation)

      choices <- as.character(pull(choices))

      virtualSelectInput(
        inputId = ns("implementation"),
        label = strong("6. Practice Implementation"),
        choices = sort(unique(choices)),
        multiple = FALSE,
        position = "bottom",
        optionsCount = 5,
        search = TRUE
      )
    })

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

    # create reactive df for full table and plot ---------------------------------------------

    # prepare data for table

    df <- data.frame(
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

    rv <- reactiveValues(x = df)

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

    # prepare data for summarize by county table ------------------------------------------

    summary_df <- data.frame(
      "MLRA" = character(),
      "County" = character(),
      "# of Practice Implementations" = numeric(),
      "Total Acres" = numeric(),
      "Total Greenhouse Gases" = numeric()
    )

    summary_county <- reactive({
      req(rv$df)
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

    # total acres info box ----------------------------------------------------------

    value_acres <- reactive({
      value_acres <- summary_county() %>%
        select("Total Acres") %>%
        as.vector() %>%
        sum() %>%
        prettyNum(big.mark = ",")
    })

    output$total_acres <- renderInfoBox({
      infoBox("Total Acres",
        value = paste(value_acres(), "Acres"),
        icon = icon("seedling"),
        color = "yellow",
        fill = TRUE,
        width = 6
      )
    })

    # total ghg info box ----------------------------------------------------------

    value_ghg <- reactive({
      value_ghg <- summary_county() %>%
        select("Total Greenhouse Gases") %>%
        as.vector() %>%
        sum() %>%
        prettyNum(big.mark = ",")
    })

    output$total_ghg <- renderInfoBox({
      infoBox("Total GHG Reductions",
        value = paste(value_ghg(), "MT CO2eq/yr"),
        icon = icon("globe"),
        color = "teal",
        fill = TRUE,
        width = 6
      )
    })

    # add, edit, or delete rows ------------------------------------------------------

    # add new row to table

    observeEvent(input$add, {
      req(
        input$county,
        input$class,
        input$practice,
        input$acres >= 1,
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
              actionButton(ns("confirm"), label = "Yes")
            ), easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Warning",
            paste("Please select the row(s) that you want to remove.
                  You can only select rows from the table."),
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

    # render tables ------------------------------------------------------------

    # full table

    output$table <- DT::renderDT({
      isolate(rv$df)
      fct_table(df, type = "estimate")
    })

    proxy_full <- DT::dataTableProxy("table")

    observe({
      DT::replaceData(proxy_full, rv$df, rownames = FALSE)
    })

    # summary table

    output$summary <- DT::renderDT({
      isolate(rv$df)
      fct_table(summary_df, type = "summary")
    })

    proxy_summary <- DT::dataTableProxy("summary")

    observe({
      DT::replaceData(proxy_summary, summary_county(), rownames = FALSE)
    })

    # render plot -------------------------------------------------------------

    filtered_plot <- reactive({
      req(rv$df)
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
      if (is.null(rv$df)) {
        validate("Add some data to see the graph.")
      }
      if (dplyr::n_distinct(filtered_plot()$implementation) > 20 ||
        nrow(filtered_plot()) > 60) {
        validate("The plot is too cluttered. Please remove some selections.")
      }
      fct_plot(filtered_plot(), type = "estimate", error_bar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_estimate_ui("estimate_1")

## To be copied in the server
# mod_estimate_server("estimate_1")
