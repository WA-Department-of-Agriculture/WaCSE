#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd

# TODO: keep trying to fix server mods for table and plot
#       create mod for map

app_server <- function(input, output, session) {
  # Your application server logic

  # render UI filter elements and reactive df-----------------------------------------------

  filtered_df <- mod_filters_server("filters")

  observeEvent(input$reset, {
    shinyjs::reset("form")
  })

  # render table ------------------------------------------------------------

  filtered <- reactive({
    filtered <- fct_table_filter(filtered_df()) %>%
      mutate(across(where(is.numeric), ~ replace(., is.na(.), "Not estimated")))
    })

  output$table <- DT::renderDataTable(fct_table(data = filtered(), type = "explore"))

  # render plot -------------------------------------------------------------

  ghg_type <- reactive({
    input$ghg_type
  })

  filtered_plot <- reactive({
    subset(filtered_df(), ghg_type == input$ghg_type)
  })

  output$plot <- ggiraph::renderGirafe({
    req(filtered_plot())
    fct_plot(filtered_plot(), ghg_type())
  })

  # render map --------------------------------------------------------------

  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      setView(
        lng = -119.5,
        lat = 47.5,
        zoom = 7
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery)
  })

  # render estimate DT tab ------------------------------------------------------

  mod_editableDT_server("editableDT")
}
