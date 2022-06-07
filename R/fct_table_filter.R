#' table_filter
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
