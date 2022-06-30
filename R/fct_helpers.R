#' font
#'
#' @description A fct function to prepare a font
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_font <- function(id) {
  gfonts::setup_font(
    id = "poppins",
    output_dir = "inst/app/www",
    variants = c("regular", "italic", "700", "700italic")
  )

  gfonts::use_font(id = "poppins", "inst/app/www/css/poppins.css")
}

#' theme
#' @description define theme for shiny app
#' @noRd

theme <- bslib::bs_theme(
  version = 5,
  primary = "#253e90", secondary = "#61c3ee", success = "#489739", warning = "#ffb04c",
  base_font = bslib::font_google("Poppins"), font_scale = NULL,
  `enable-shadows` = TRUE, `enable-rounded` = TRUE,
  bootswatch = "lumen"
)

#' fct_wrap
#'
#' @description wrap text
#'
#' @param x what text to be wrapped
#' @param width is how wide you want the string before it wraps
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

fct_wrap <- function(x, width) {
  stringr::str_wrap(x, width = width)
}

#' fct_sumAcres
#'
#' @description sum acres
#'
#' @param county which counties the user selects
#' @param type cropland, pasture, or other. Other contains conservation, fallow, research, unknown, and wildlife feed.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

fct_sumAcres <- function(county, type) {

  # subset by county

  filterCounty <- crop_data %>%
    subset(County %in% county)

  # subset by type

  filtered_df <- switch(type,
    cropland = {
      subset(
        filterCounty,
        !CropGroup %in% c("Developed", "Other", "Pasture", "Shellfish", "Turfgrass")
      )
    },
    pasture = {
      subset(
        filterCounty,
        CropGroup == "Pasture"
      )
    },
    other = {
      subset(
        filterCounty,
        CropGroup == "Other"
      )
    }
  )

  totalAcres <- format(round(sum(filtered_df$ExactAcres), 0), big.mark = ",")

  totalAcres
}

#' fct_table_filter
#'
#' @description A fct function to filter and pivot data for datatable
#'
#' @param data is the dataframe to filter and pivot
#'
#' @return data The return value, if any, from executing the function.
#'
#' @noRd

fct_table_filter <- function(data) {
  data <- data %>%
    select(
      "mlra",
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
}
