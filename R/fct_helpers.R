#' register font
#'

# gfonts::setup_font(
#   id = "source-sans-pro",
#   output_dir = "inst/app/www",
#   variants = c("regular", "italic", "700", "700italic")
# )
#
# gfonts::use_font(id = "source-sans-pro",
#                  css_path = "inst/app/www/css/source-sans-pro.css",
#                  selector = ".dummy-selector")
#
# if (!ggiraph::font_family_exists("Source Sans Pro")) {
#   systemfonts::register_font(name = "Source Sans Pro",
#                 plain = list("inst/app/www/fonts/source-sans-pro-v21-latin-regular.woff", 0),
#                 bold = list("inst/app/www/fonts/source-sans-pro-v21-latin-700.woff", 0),
#                 italic = list("inst/app/www/fonts/source-sans-pro-v21-latin-italic.woff", 0),
#                 bolditalic = list("inst/app/www/fonts/source-sans-pro-v21-latin-700italic.woff", 0)
#   )
# }

#' theme
#' @description define theme for shiny app
#' @noRd

theme <- bslib::bs_theme(
  version = 5,
  primary = "#f2cc8f", secondary = "#f2cc8f", success = "#81B29A",
  warning = "#f2cc8f", danger = "#E07A5F", royal = "#3d405b",
  base_font = bslib::font_google("Source Sans Pro"), font_scale = NULL,
  `enable-shadows` = TRUE, `enable-rounded` = TRUE, bootswatch = "lumen"
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


#' fct_tableFilter
#'
#' @description A fct function to filter and pivot data for datatable
#'
#' @param data is the dataframe to filter and pivot
#'
#' @return data The return value, if any, from executing the function.
#'
#' @noRd

fct_tableFilter <- function(data) {
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

#' fct_helpBtn
#'
#' @description A function to add a help button with the question mark icon
#'
#' @param id The input slot that will be used to access the value.
#'
#'
#' @noRd

fct_helpBtn <- function(id) {
  shinyWidgets::actionBttn(
    inputId = id,
    label = NULL,
    icon = icon("question"),
    style = "material-circle",
    size = "xs",
  )
}

#' fct_helpModal
#'
#' @description A function to render a help modal from the fct_helpBtn
#'
#' @param md The name of the markdown file to include. Must be called within observeEvent(input$helpBtn)
#' @examples
#' observeEvent(input$filterHelp, {
#'   fct_helpModal("exploreFilter")
#' })
#'
#' @noRd

fct_helpModal <- function(md) {
  showModal(modalDialog(
    includeMarkdown(normalizePath(paste0("inst/app/www/rmd/", md, ".md"))),
    easyClose = TRUE
  ))
}
