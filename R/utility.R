#' Returns all unique values for a specified column in a data frame
#'
#' @param df Data frame
#' @param column Column name
#'
#' @return Vector with unique values in that column, excluding NA
#' @keywords internal
#'
#' @examples
#' get_unique(df_fake, report_nr)
get_unique <- function(df, column) {
  unique_values <- df %>%
    dplyr::filter(!is.na({{ column }})) %>%
    dplyr::distinct({{ column }}) %>%
    dplyr::pull({{ column }})

  return(unique_values)
}
