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

  # Render UI filter elements and reactive df-----------------------------------------------

  filtered_df <- mod_filters_server("filters")

  observeEvent(input$reset, {
    shinyjs::reset("form")
  })

  # Render table ------------------------------------------------------------

  output$table <- DT::renderDataTable(fct_table(filtered_df()))

  # Render plot -------------------------------------------------------------

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

  # Render map --------------------------------------------------------------

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
