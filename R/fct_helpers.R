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
  stringr::str_wrap(x, width = 25)
}

#' fct_label
#'
#' @description label y-axis of plot
#'
#' @param ghg_type from the comet_wa df and selectize input from mod_plot
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
fct_label <- function(ghg_type) {
  if (ghg_type == "co2") {
    return("Carbon Dioxide")
  }
  if (ghg_type == "n2o") {
    return("Nitrous Oxide")
  }
  if (ghg_type == "ch4") {
    return("Methane")
  }
  if (ghg_type == "total.ghg.co2") {
    return("Total Greenhouse Gases")
  }
}

#' make UI. doesn't work.
#'
#' @description make a selectize UI
#'
#' @param id unique id
#' @param subset which column to subset
#' @param label for selectize input
#' @param num_choice how many choices to select
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

fct_makeUI <- function(id, subset, label, num_choice) {
  output$id <- renderUI({
    choices <- unique(comet_tags) %>%
      subset(subset %in% input$subset) %>%
      select(id)

    choices <- as.character(pull(choices))

    selectizeInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = choices[num_choice],
      multiple = TRUE,
      options = list(
        maxItems = 3,
        plugins = list("remove_button")
      )
    )
  })
}
