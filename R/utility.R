#' Filter data frame using the Destatis table code, discarding all columns that
#' only consist of NA.
#'
#' @param df Data frame with \code{tablename} column
#' @param tablename Destatis table code
#'
#' @return Filtered data frame with all NA-columns dropped
#' @export
#'
#' @examples
#' \dontrun{
#' filter_destatis_code(df = db_nrw_213, tablename = "21391KF061")
#' }
filter_destatis_code <- function(df, tablename) {
  df_filtered <- df %>%
    dplyr::filter(
      tablename == {{tablename}}
    ) %>%
    purrr::discard(
      ~all(is.na(.))
    )

  return(df_filtered)
}


#' Returns all unique values for a specified column in a data frame
#'
#' @param df Data frame
#' @param column Column name
#'
#' @return Vector with unique values in that column, excluding NA
#'
#' @examples
#' get_unique(df, report_nr)
get_unique <- function(df, column) {
  unique_values <- df %>%
    dplyr::filter(!is.na({{ column }})) %>%
    dplyr::distinct({{ column }}) %>%
    dplyr::pull({{ column }})

  return(unique_values)
}
