#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  filtered_df <- mod_selectizeGroup_server("filters")

  output$explore <- DT::renderDataTable(
    filtered_df() %>%
      select(
        county,
        class,
        cps_name,
        planner_implementation,
        co2_mean,
        n2o_mean,
        soil_carbon_co2,
        total_ghg_co2
      ) %>%
      rename(
        County = county,
        Class = class,
        Practice = cps_name,
        Implementation = planner_implementation,
        "CO2 Mean" = co2_mean,
        "N2O Mean" = n2o_mean,
        "Soil Carbon Mean" = soil_carbon_co2,
        "Total GHG" = total_ghg_co2
      ),
    rownames = FALSE,
    extensions = c("Scroller", "RowGroup"),
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = "10px", targets = "_all")),
      dom = "t",
      rowGroup = list(dataSrc = 0),
      scrollX = TRUE,
      scrollY = 500
    ),
    selection = "none"
  )

}



