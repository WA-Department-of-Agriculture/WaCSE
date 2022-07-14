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
  primary = "#f2cc8f", secondary = "#f2cc8f", success = "#81B29A",
  warning = "#f2cc8f", danger = "#E07A5F", base_font = bslib::font_google("Poppins"),
  font_scale = NULL, `enable-shadows` = TRUE, `enable-rounded` = TRUE, bootswatch = "lumen"
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
