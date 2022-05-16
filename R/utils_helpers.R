#' select columns
#'
#' @description select columns for data table
#'
#' @param x what text to be wrapped
#' @param width is how wide you want the string before it wraps
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

wrap <- function(x, width) {
  stringr::str_wrap(x, width = 25)
}

