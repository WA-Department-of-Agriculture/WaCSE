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

  output$table <- DT::renderDT(fct_table(data = filtered(), type = "explore"))

  # render plot -------------------------------------------------------------

  filtered_plot <- reactive({
    filtered_df() %>%
      filter(ghg_type == "total.ghg.co2")
  })

  output$plot <- ggiraph::renderGirafe({
    req(filtered_plot())
    if (dplyr::n_distinct(filtered_plot()$implementation) > 10 ||
      nrow(filtered_plot()) > 40) {
      validate("The plot is too cluttered. Please remove some selections.")
    }
    fct_plot(filtered_plot(), "total.ghg.co2", error_bar = TRUE, tt = "noAcres")
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
