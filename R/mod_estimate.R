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

mod_estimate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,

        # inputs box ---------------------------------------------------------

        shinydashboard::box(
          title = tagList(
            span(strong("Add conservation practices")),
            span(fct_helpBtn(ns("addHelp")))
          ),
          width = NULL,
          status = "primary",
          collapsible = TRUE,
          solidHeader = TRUE,
          shinyWidgets::virtualSelectInput(
            inputId = ns("county"),
            label = strong("Step 1. County"),
            placeholder = "Select county",
            choices = unique(comet_wa$county),
            selected = " ",
            multiple = FALSE,
            position = "bottom",
            search = TRUE,
            optionsCount = 5
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("class"),
            label = span(
              strong("Step 2. Conservation Class"),
              br(),
              helpText("Select the category that describes the practices
                       you are interested in.")
            ),
            placeholder = "Select conservation class",
            choices = sort(unique(comet_tags$class)),
            selected = " ",
            multiple = FALSE,
            position = "bottom"
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("practice"),
            label = span(
              strong("Step 3. Conservation Practice"),
              br(),
              helpText("Select the NRCS conservation practice standard
                       (CPS) you are interested in.")
            ),
            placeholder = "Select conservation practice",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            position = "bottom",
            search = TRUE,
            optionsCount = 5
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("irrigation"),
            label = span(
              strong("Step 4. Irrigation Type"),
              br(),
              helpText("Select how the current system is irrigated.")
            ),
            placeholder = "Select irrigation type",
            choices = NULL,
            multiple = FALSE,
            selected = NULL,
            position = "bottom",
            showValueAsTags = TRUE
          ),
          shinyWidgets::virtualSelectInput(
            inputId = ns("implementation"),
            label = span(
              strong("Step 5. Practice Implementation"),
              br(),
              helpText("Select how the practice will be implemented.")
            ),
            placeholder = "Select practice implementation",
            choices = NULL,
            multiple = FALSE,
            selected = NULL,
            position = "bottom",
            search = TRUE,
            showValueAsTags = TRUE
          ),
          numericInput(
            inputId = ns("acres"),
            label = span(
              strong("Step 6. Number of Acres"),
              br(),
              helpText("Select the number of acres this
                       practice will be used on.")
            ),
            value = "1",
            min = 1,
            max = 10000000
          ),
          actionButton(
            inputId = ns("add"),
            label = "Add",
            class = "btn-success",
            icon = icon("plus")
          ),
          actionButton(
            inputId = ns("remove"),
            label = "Remove",
            class = "btn-danger",
            icon = icon("minus")
          )
        )
      ), column(
        width = 8,
        fluidRow(

          # view estimate box -------------------------------------------------

          shinydashboard::box(
            title = tagList(
              span(strong("View your GHG reduction estimate")),
              span(fct_helpBtn(id = ns("viewHelp")))
            ),
            width = NULL,
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            tabsetPanel(
              id = ns("tabs"),
              type = "pills",
              tabPanel("Table",
                icon = icon("table"),
                br(),
                strong(
                  HTML(
                    "All NRCS conservation classes, practices, and
                  implementations are described in the
                   <a href='http://bfuels.nrel.colostate.edu/beta/COMET-Planner_Report_Final.pdf'
                   target = '_blank'>COMET-Planner Final Report PDF</a>."
                  )
                ),
                rep_br(2),
                shinycssloaders::withSpinner(
                  DT::DTOutput(outputId = ns("table"))
                )
              ),
              tabPanel("Bar Graph",
                icon = icon("chart-bar"),
                br(),
                shinycssloaders::withSpinner(
                  ggiraph::girafeOutput(outputId = ns("plot"))
                )
              )
            )
          )
        ),
        fluidRow(

          # summary and download box -----------------------------------------

          shinydashboard::box(
            title = tagList(
              span(strong("Summarize and download your GHG reduction estimate")),
              span(fct_helpBtn(id = ns("summarizeHelp")))
            ),
            width = NULL,
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            tabsetPanel(
              type = "pills",
              tabPanel(
                title = "Summary",
                icon = icon("list"),
                br(),
                fluidRow(
                  htmlOutput(outputId = ns("impact")),
                  shinydashboard::valueBoxOutput(outputId = ns("total_acres")),
                  shinydashboard::valueBoxOutput(outputId = ns("total_ghg")),
                  shinycssloaders::withSpinner(
                    DT::DTOutput(outputId = ns("summary"))
                  )
                )
              ),
              tabPanel(
                title = "Download Report",
                icon = icon("file-export"),
                br(),
                textInput(
                  inputId = ns("name"),
                  label = "Organization or Farm Name"
                ),
                textInput(
                  inputId = ns("project"), "Project Name"
                ),
                downloadButton(
                  outputId = ns("report"),
                  label = "Download",
                  class = "btn-success"
                )
              )
            )
          )
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

    # help modals -------------------------------------------------------------

    # modal for add practice help

    observeEvent(input$addHelp, {
      fct_helpModal("estimateAdd")
    })

    # # modal for explore help

    observeEvent(input$viewHelp, {
      fct_helpModal("estimateView")
    })

    # modal for summarize help

    observeEvent(input$summarizeHelp, {
      fct_helpModal("estimateSummarize")
    })

    # update UI inputs -----------------------------------------------

    # practice input

    observeEvent(
      eventExpr = {
        input$class
      },
      handlerExpr = {
        choices <- unique(
          comet_wa$practice[comet_wa$class %in% input$class]
        )

        shinyWidgets::updateVirtualSelect(
          inputId = "practice",
          choices = sort(choices),
          selected = input$practice
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
        choices <- unique(
          comet_wa$irrigation[comet_wa$class %in% input$class &
            comet_wa$practice %in% input$practice]
        )

        shinyWidgets::updateVirtualSelect(
          inputId = "irrigation",
          choices = sort(choices),
          selected = input$irrigation
        )
      }
    )

    # render implementation input

    observeEvent(
      eventExpr = {
        input$class
        input$practice
        input$irrigation
      },
      handlerExpr = {
        choices <- unique(
          comet_wa$implementation[comet_wa$class %in% input$class &
            comet_wa$practice %in% input$practice &
            comet_wa$irrigation %in% input$irrigation]
        )

        shinyWidgets::updateVirtualSelect(
          inputId = "implementation",
          choices = sort(choices),
          selected = input$implementation
        )
      }
    )

    # give warning if user selects acres <1

    observeEvent(input$acres, {
      req(input$acres)
      condition <- dplyr::between(input$acres, 1, 10000000)
      shinyFeedback::feedbackDanger(
        "acres", !condition,
        "Please input at least one acre but not more than 10,000,000.",
        color = "#b50000"
      )
      return(input$acres)
    })

    # only show Remove action button on the Table tab

    observeEvent(input$tabs, {
      if (input$tabs == "Table") {
        shinyjs::runjs(
          "document.getElementById('estimate_tab-remove')
          .style.visibility = 'visible';"
        )
      } else {
        shinyjs::runjs(
          "document.getElementById('estimate_tab-remove')
          .style.visibility = 'hidden';"
        )
      }
    })

    # create reactive df for full table and plot -------------------------------

    # prepare data for table

    df <- data.frame(
      "mlra" = character(),
      "county" = character(),
      "class" = character(),
      "practice" = character(),
      "implementation" = character(),
      "acres" = numeric(),
      "co2" = numeric(),
      "n2o" = numeric(),
      "ch4" = numeric(),
      "totalGHG" = numeric()
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

      filtered <- fct_tableFilter(filtered)

      return(filtered)
    })

    # add, edit, or delete rows ------------------------------------------------

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
        "mlra" = filtered()$mlra,
        "county" = filtered()$county,
        "class" = filtered()$class,
        "practice" = filtered()$practice,
        "implementation" = filtered()$implementation,
        "acres" = input$acres,
        "co2" = input$acres * filtered()$co2,
        "n2o" = input$acres * filtered()$n2o,
        "ch4" = input$acres * filtered()$ch4,
        "totalGHG" = input$acres * filtered()$total.ghg.co2
      )

      rv$df <- rbind(rv$df, tmp) %>% unique()

      return(rv$df)
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
              actionButton(
                inputId = ns("confirm"),
                label = "Yes",
                class = "btn-danger"
              )
            ),
            easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Warning",
            paste("Please select the row(s) that you want to remove."),
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

    # prepare summary data  ------------------------------------------

    summary_df <- data.frame(
      "mlra" = character(),
      "county" = character(),
      "uniqueImpl" = numeric(),
      "acres" = numeric(),
      "totalGHG" = numeric()
    )

    summary_county <- reactive({
      req(rv$df)
      summary_county <- rv$df %>%
        mutate(
          acres = as.numeric(acres),
          totalGHG = as.numeric(totalGHG)
        ) %>%
        group_by(mlra, county) %>%
        summarize(
          "uniqueImpl" = dplyr::n_distinct(implementation),
          "acres" = sum(acres),
          "totalGHG" = sum(totalGHG)
        ) %>%
        as.data.frame()
      return(summary_county)
    })

    # total acres value box ---------------------------------------------------

    value_acres <- reactive({
      req(rv$df)
      value_acres <- rv$df %>%
        dplyr::select(acres) %>%
        as.vector() %>%
        sum() %>%
        format(big.mark = ",")
    })

    output$total_acres <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Total Acres",
        value = paste(value_acres(), "Ac"),
        icon = icon("leaf"),
        color = "green",
        width = NULL
      )
    })

    # total ghg value box ------------------------------------------------------

    value_ghg <- reactive({
      req(rv$df)
      value_ghg <- rv$df %>%
        dplyr::select(totalGHG) %>%
        as.vector() %>%
        sum() %>%
        format(big.mark = ",")
      return(value_ghg)
    })

    output$total_ghg <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "Total GHG Reductions",
        value = paste(value_ghg(), "MT CO2eq/yr"),
        icon = icon("globe-americas"),
        color = "blue",
        width = NULL
      )
    })

    # instructions for entering total GHG reductions into impact tab ----------

    output$impact <- renderUI({
      req(rv$df)

      HTML(paste0("<p> Visit the Understand your impact tab at the top of this
                  page to learn about what this GHG reduction means.</p>"))
    })

    # render tables ------------------------------------------------------------

    # full table

    output$table <- DT::renderDT({
      fct_table(df, type = "estimate")
    })

    proxy_full <- DT::dataTableProxy("table")

    observe({
      req(rv$df)
      data <- as.data.frame(rv$df)
      DT::replaceData(proxy_full, data, rownames = FALSE)
    })

    # summary table

    output$summary <- DT::renderDT({
      fct_table(summary_df, type = "summary")
    })

    proxy_summary <- DT::dataTableProxy("summary")

    observe({
      data <- as.data.frame(summary_county())
      DT::replaceData(proxy_summary, data, rownames = FALSE)
    })

    # render plot -------------------------------------------------------------

    filtered_plot <- reactive({
      req(rv$df)
      rv$df %>%
        dplyr::select(
          mlra = mlra,
          county = county,
          abbr = implementation,
          implementation = implementation,
          acres = acres,
          mean = totalGHG
        ) %>%
        dplyr::mutate(
          acres = as.numeric(acres),
          mean = as.numeric(mean)
        )
    })

    output$plot <- ggiraph::renderGirafe({
      if (is.null(rv$df)) {
        validate("Add some data to see the graph.")
      }

      if (dplyr::n_distinct(filtered_plot()$implementation) > 10 ||
        nrow(filtered_plot()) > 30) {
        validate("The graph is too cluttered. Please remove some selections.")
      }

      fct_plot(filtered_plot(), type = "estimate", error_bar = FALSE)
    })

    # download report ---------------------------------------------------------

    output$report <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_WaCSE_Report.pdf")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it,
        # in case we don't have write permissions to the current working dir
        # (which can happen when deployed).
        withProgress(
          message = "Preparing your report.",
          detail = " This could take a while...",
          {
            tempReport <- file.path(tempdir(), "WaCSE_Report.Rmd")
            file.copy(normalizePath("inst/app/www/rmd/WaCSE_Report.Rmd"),
              tempReport,
              overwrite = TRUE
            )
            fct_ghgEq <- file.path(tempdir(), "fct_ghgEq.R")
            file.copy(normalizePath("R/fct_ghgEq.R"),
              fct_ghgEq,
              overwrite = TRUE
            )

            incProgress(0.1)

            # Set up parameters to pass to Rmd document
            params <- list(
              name = input$name,
              project = input$project,
              data = rv$df,
              summary = summary_county(),
              plot = fct_plot(filtered_plot(),
                type = "download",
                error_bar = FALSE
              )
            )

            incProgress(0.5)

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the
            # document from the code in this app).
            rmarkdown::render(tempReport,
              output_file = file,
              params = params,
              envir = new.env(parent = globalenv())
            )

            incProgress(1)
          }
        )
      }
    )
  })
}

## To be copied in the UI
# mod_estimate_ui("estimate_1")

## To be copied in the server
# mod_estimate_server("estimate_1")
