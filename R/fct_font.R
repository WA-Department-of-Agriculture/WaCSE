#' font
#'
#' @description A fct function to prepare a font
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_font <- function(id) {

gfonts::setup_font(
  id = "montserrat",
  output_dir = "inst/app/www",
  variants = c("regular", "italic", "700", "700italic")
)

gfonts::use_font(id = "montserrat", "inst/app/www/css/montserrat.css")

}
