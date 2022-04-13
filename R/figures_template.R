#' Get code chunks for plotting all figures
#'
#' @param df Data frame with columns `figure_count`
#'
#' @return Data frame with columns `figure_count`, `chunk_heading`,
#'         `chunk_subheading`, `chunk_figure_df` and `chunk_plot_figure`
#' @export
#'
#' @examples
#' get_figure_chunk_df(RUBer::df_example %>% dplyr::filter(report_nr == 1L))
get_figure_chunk_df <- function(
  df,
  function_call = "RUBer::plot_figure"
) {
  chunk_parameters <- df %>%
    dplyr::filter(
      !is.na(
        .data[["figure_count"]]
      )
    ) %>%
    dplyr::distinct(
      .data[["figure_count"]],
      .data[["is_heading"]],
      .data[["is_subheading"]],
      .data[["heading"]],
      .data[["subheading"]],
      .data[["figure_caption"]],
      .data[["figure_height"]]
    ) %>%
    dplyr::mutate(
      chunk_label = stringr::str_pad(
        .data[["figure_count"]],
        width = 3L,
        pad = "0"
      ),
      function_parameters = glue::glue(
        "df, figure_count == {figure_count}L"
      ),
      .before = .data[["is_heading"]]
    ) %>%
    dplyr::mutate(
      chunk_heading = dplyr::if_else(
        .data[["is_heading"]],
        tpl_heading(
          chunk_label = .data[["chunk_label"]],
          heading = .data[["heading"]]
        ),
        list(
          NA_character_
        )
      ),
      chunk_subheading = dplyr::if_else(
        .data[["is_subheading"]],
        tpl_subheading(
          chunk_label = .data[["chunk_label"]],
          subheading = .data[["subheading"]]
        ),
        list(
          NA_character_
        )
      ),
      chunk_figure_df = tpl_get_figure_df(
        chunk_label = .data[["chunk_label"]],
        function_params = .data[["function_parameters"]]
      ),
      chunk_plot_figure = tpl_plot_figure(
        chunk_label = .data[["chunk_label"]],
        figure_caption = .data[["figure_caption"]],
        figure_height = .data[["figure_height"]]
      )
    ) %>%
    dplyr::select(
      1L,
      tidyselect::last_col(
        offset = 3L
      ) : tidyselect::last_col()
    )

  return(chunk_parameters)
}

#' Turn data frame obtained by `get_figure_chunk_df` into character vector of
#' code chunks
#'
#' @param df Data frame obtained by `get_figure_chunk_df`
#'
#' @return Vector with chunk texts
#' @export
#'
#' @examples
#' get_figure_chunk_text(get_figure_chunk_df(RUBer::df_example %>% dplyr::filter(report_nr == 1L)))
get_figure_chunk_text <- function(
  df
) {
  chunk_text <- df %>%
    tidyr::pivot_longer(
      2L : tidyselect::last_col(),
      values_to = "chunk_text"
    ) %>%
    dplyr::select(
      .data[["chunk_text"]]
    ) %>%
    dplyr::filter(
      !is.na(
        .data[["chunk_text"]]
      )
    ) %>%
    unlist()

  return(chunk_text)
}
