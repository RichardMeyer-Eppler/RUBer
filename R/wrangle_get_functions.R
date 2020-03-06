#' Get figure caption
#'
#' @param df Dataframe bzw. Tibble
#'
#' @return Caption as character
#' @export
#'
#' @examples
#' get_figure_caption(df)
get_figure_caption <- function(param_list) {
  txt <- param_list[["figure_txt"]]

  fig_type_id <- param_list[["figure_type_id"]]
  fig_id <- param_list[["figure_id"]]
  dg_txt <- param_list[["degree_group_txt"]]
  sa_txt <- param_list[["subject_area_txt"]]
  sg_txt <- param_list[["subject_group_txt"]]
  rep_id <- param_list[["report_id"]]

  txt_paranthesis <- if (fig_type_id == 1 |
                         fig_type_id == 4 |
                         fig_id %in% c(2,3,5,6)) {
    NULL
  } else if (fig_type_id == 2L &
             fig_id == 4L &
             rep_id == "2018_M_ED") {
    sa_txt
  } else if (fig_type_id == 2L &
             fig_id == 4L &
             rep_id == "2018_FG"
  ) {
    paste(sg_txt, dg_txt, sep = ", ")
  } else {
    dg_txt
  }

  txt_paranthesis <- if (!is.null(txt_paranthesis)) {
    paste0(" (", txt_paranthesis, ")")
  }

  caption <- paste0(txt, txt_paranthesis)

  return(caption)
}

#' Prepare data frame for plotting of a particular figure by filtering and
#'    setting factors.
#'
#' @param df Dataf rame
#' @param figure_count Figure count of the figure
#'
#' @return Data frame
#' @export
#'
#' @examples
#' get_figure_df(df, figure_count = 16)
get_figure_df <- function(df, figure_count) {
  figure_df <- dplyr::filter(
    df,
    figure_count == {{ figure_count }}
  )

  figure_df <- RUB::set_factors(figure_df)

  return(figure_df)
}

#' Get figure height
#'
#' @param param_list Data frame
#' @param lower_bound Minimum height (default = 2.25)
#' @param upper_bound Maximum height (default = 9.6)
#'
#' @return Figure height in inches
#' @export
#'
#' @examples
#' get_figure_height(param_list)
get_figure_height <- function(param_list, lower_bound = 2, upper_bound = 9.6) {
  figure_type_id <- param_list[["figure_type_id"]]
  facet_count <- param_list[["facet_count"]]
  x_facet_count <- param_list[["x_facet_count"]]

  if(figure_type_id == 2)  {
    x_facet_count <- 5 * facet_count
  }

  figure_height <- (x_facet_count * 0.75) + (facet_count * 0.15)

  if (figure_height > upper_bound) {
    figure_height <- upper_bound
  }

  else if (figure_height < lower_bound) {
    figure_height <- lower_bound
  }

  return(figure_height)
}
