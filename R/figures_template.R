#' Get code chunks for plotting all figures
#'
#' @param df Data frame with columns `figure_nr`
#' @param font_family Character, the font family to use for all plots, defaults
#'     to `get_font_df()[["family"]]`
#' @param function_call Character, the function call for each plot chunk,
#'     defaults to `RUBer::plot_figure`
#'
#' @return Data frame with columns `figure_nr`, `chunk_heading`,
#'     `chunk_subheading`, `chunk_figure_df` and `chunk_plot_figure`
#' @export
#'
#' @examples
#' get_figure_chunk_df(RUBer::df_example %>% dplyr::filter(report_nr == 1L))
get_figure_chunk_df <- function(
  df,
  font_family = get_font_df()[["family"]],
  function_call = "RUBer::plot_figure"
) {
  chunk_parameters <- df %>%
    dplyr::filter(
      !is.na(
        .data[["figure_nr"]]
      )
    ) %>%
    dplyr::distinct(
      .data[["figure_nr"]],
      .data[["is_heading"]],
      .data[["is_subheading"]],
      .data[["heading"]],
      .data[["subheading"]],
      .data[["figure_caption"]],
      .data[["figure_height"]]
    ) %>%
    dplyr::mutate(
      chunk_label = stringr::str_pad(
        .data[["figure_nr"]],
        width = 3L,
        pad = "0"
      ),
      function_parameters = glue::glue(
        "df, figure_nr == {figure_nr}L"
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
        function_call = function_call,
        chunk_label = .data[["chunk_label"]],
        figure_caption = .data[["figure_caption"]],
        figure_height = .data[["figure_height"]],
        font_family = font_family
      )
    ) %>%
    dplyr::select(
      1L,
      dplyr::last_col(
        offset = 3L
      ) : dplyr::last_col()
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
      2L : dplyr::last_col(),
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
