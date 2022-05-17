#' Returns a character vector for the code chunk to retrieve the figure data
#' frame
#'
#' @param chunk_label Character, chunk label
#' @param function_call Character, defaults to `dplyr::filter`
#' @param function_params Character
#'
#' @return List of character vectors with chunk texts
#' @export
#'
#' @example inst/examples/tpl_get_figure_df.R
tpl_get_figure_df <- function(
  chunk_label,
  function_params,
  function_call = "dplyr::filter"
) {

  get_chunk_texts <- function(
    chunk_label,
    function_params,
    function_call
  ) {
    chunk_texts <- c(
      glue::glue(
        "\n```{r prep-fig-<<chunk_label>>, include = FALSE}",
        .open = "<<",
        .close = ">>",
        .trim = FALSE
      ),
      glue::glue(
        "df_fig <- <<function_call>>(<<function_params>>)",
        .open = "<<",
        .close = ">>"
      ),
      "```"
    )

    return(chunk_texts)
  }

  template <- purrr::map2(
    chunk_label,
    function_params,
    get_chunk_texts,
    function_call = function_call
  )

  return(template)
}

#' Returns a character vector for the code chunk to insert a heading
#'
#' @param chunk_label Character, chunk label
#' @param heading Character, heading text
#' @param level Integer, level of the heading, defaults to `1L`
#'
#' @return List of character vectors with chunk texts
#' @export
#'
#' @example inst/examples/tpl_heading.R
tpl_heading <- function(
  chunk_label,
  heading,
  level = 1L
) {

  level_string <- paste0(
    rep(
      "#",
      times = level
    ),
    collapse = ""
  )

  get_chunk_texts <- function(
    chunk_label,
    heading,
    level_string
  ) {
    chunk_texts <- list(
      c(
        glue::glue(
          "\n```{r heading-fig-<<chunk_label>>, results= 'asis'}",
          .open = "<<",
          .close = ">>",
          .trim = FALSE
        ),
        glue::glue(
          "cat(paste(\"{level_string}\", \"{heading}\"))"
        ),
        "```"
      )
    )
  }

  template <- purrr::map2(
    chunk_label,
    heading,
    get_chunk_texts,
    level_string
  )

  return(template)
}

#' Returns a character vector for the code chunk to insert a subheading
#'
#' @param chunk_label Character, chunk label
#' @param subheading Character, subheading text
#' @param level Integer, level of the heading, defaults to `2L`
#'
#' @return List of character vectors with chunk texts
#' @export
#'
#' @example inst/examples/tpl_subheading.R
tpl_subheading <- function(
  chunk_label,
  subheading,
  level = 2L
) {

  level_string <- paste0(
    rep(
      "#",
      times = level
    ),
    collapse = ""
  )

  get_chunk_texts <- function(
    chunk_label,
    subheading,
    level_string
  ) {
    list(
      c(
        glue::glue(
          "\n```{r subheading-fig-<<chunk_label>>, results= 'asis'}",
          .open = "<<",
          .close = ">>",
          .trim = FALSE
        ),
        glue::glue(
          "cat(paste(\"{level_string}\", \"{subheading}\"))"
        ),
        "```"
      )
    )
  }

  template <- purrr::map2(
    chunk_label,
    subheading,
    get_chunk_texts,
    level_string
  )

  return(template)
}

#' Returns a character vector for the code chunk to plot a figure
#'
#' @param chunk_label Character, chunk label
#' @param figure_caption Character, figure caption
#' @param figure_height Numeric, figure height in inches
#' @param font_family Character, the font family to use for all plots, defaults
#'     to `get_font_df()[["family"]]`
#' @param function_call Character, defaults to `RUBer::plot_figure`
#' @param tab_placeholder Character, pandoc does not support the insertion of
#'     tabs, which is why a placeholder text is needed that will get replaced
#'     with tabs in post-processing, defaults to "PLACEHOLDER_TAB"
#' @param suppress_warnings Boolean, whether to suppress the warnings generated
#'     by this code chunk, defaults to `FALSE`.
#'
#' @return List of character vectors with chunk texts
#' @export
#'
#' @example inst/examples/tpl_plot_figure.R
tpl_plot_figure <- function(
  chunk_label,
  figure_caption,
  figure_height,
  font_family = get_font_df()[["family"]],
  function_call = "RUBer::plot_figure",
  tab_placeholder = "PLACEHOLDER_TAB",
  suppress_warnings = FALSE
) {

  get_chunk_texts <- function(
    chunk_label,
    tab_placeholder,
    figure_caption,
    figure_height,
    suppress_warnings
  ) {
    list(
      c(
        glue::glue(
          "\n```{r fig-<<chunk_label>>, fig.cap = '<<tab_placeholder>><<figure_caption>>', fig.height = <<figure_height>>, fig.showtext = TRUE, warning = <<!suppress_warnings>>}",
          .open = "<<",
          .close = ">>",
          .trim = FALSE
        ),
        glue::glue(
          "{function_call}(df_fig, font_family = '{font_family}')"
        ),
        "```"
      )
    )
  }

  template <- purrr::pmap(
    list(
      chunk_label,
      tab_placeholder,
      figure_caption,
      figure_height,
      suppress_warnings
    ),
    get_chunk_texts
  )

  return(template)
}
