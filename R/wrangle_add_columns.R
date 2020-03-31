#' Add column \code{fgr_nrwbund_ltxt} to data frame
#'
#' @param df Data frame containing \code{fgr_nrwbund_id}
#' @param df_orga Data frame containing \code{fgr_nrwbund_id} and \code{fgr_nrwbund_ltxt}
#'
#' @return Data frame with additional column \code{fgr_nrwbund_ltxt}
#' @export
#'
#' @examples
#' add_fgr_nrwbund_ltxt(df)
add_fgr_nrwbund_ltxt <- function(df, df_orga) {
  orga_fgr <- df_orga %>%
    dplyr::distinct(
      fgr_nrwbund_id,
      fgr_nrwbund_ltxt
    ) %>%
    dplyr::mutate(
      fgr_nrwbund_id = as.integer(
        fgr_nrwbund_id
      ))

  df <- df %>%
    dplyr::left_join(
      orga_fgr,
      by = c("fgr_nrwbund_id")
    )

  return(df)
}

#' Adds column \code{figure_caption} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{figure_caption}
#' @export
#'
#' @examples
#' add_figure_caption(df)
add_figure_caption <- function(df)  {
  df <- df %>%
    dplyr::group_by(
      report_id,
      figure_type_id,
      figure_id,
      figure_txt,
      degree_group_txt,
      subject_area_txt,
      subject_group_txt
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      caption_params = purrr::pmap(
        list(
          "report_id" = report_id,
          "figure_type_id" = figure_type_id,
          "figure_id" = figure_id,
          "figure_txt" = figure_txt,
          "degree_group_txt" = degree_group_txt,
          "subject_area_txt" = subject_area_txt,
          "subject_group_txt" = subject_group_txt
        ),
        list
      )) %>%
    dplyr::mutate(
      figure_caption = purrr::map_chr(
        caption_params,
        RUBer::get_figure_caption
      )) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(
      cols = data
    ) %>%
    dplyr::select(
      -caption_params
    )

  return(df)
}

#' Adds column \code{figure_count} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{figure_count}
#' @export
#'
#' @examples
#' add_figure_count(df)
add_figure_count <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_nr,
      figure_id,
      figure_sort,
      degree_group_id,
      figure_filter_flag
    ) %>%
    tidyr::nest() %>%
    dplyr::group_by(
      report_nr,
      figure_id,
      figure_sort
    ) %>%
    dplyr::mutate(
      min_degree_group_flag = dplyr::if_else(
        figure_id %in% c(1, 2, 3, 5, 6),
        min(degree_group_id) == degree_group_id,
        TRUE
      )
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_filter_flag,
      min_degree_group_flag
    ) %>%
    dplyr::mutate(
      figure_count = dplyr::if_else(
        figure_filter_flag |
          !min_degree_group_flag,
        NA_integer_,
        dplyr::row_number()
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(
      cols = c(data)
    )

  ## For some figures, several degree groups are plotted in one figure. The figure count column needs to account for that.
  df <- df %>%
    dplyr::group_by(
      report_nr,
      figure_sort
    ) %>%
    dplyr::mutate(
      min_figure_count = min(
        figure_count,
        na.rm = TRUE
      )
    ) %>%
    dplyr::mutate(
      figure_count = dplyr::if_else(
        figure_id %in% c(1, 2, 3, 5, 6) &
          is.finite(
            min_figure_count
          ),
        as.double(
          min_figure_count
        ),
        as.double(
          figure_count
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -min_degree_group_flag,
      -min_figure_count
    )

  return(df)
}

#' Add column \code{figure_height} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{figure_height}
#' @export
#'
#' @examples
#' add_figure_height(df)
add_figure_height <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::mutate(
      facet_count = dplyr::n_distinct(
        facet
      )) %>%
    dplyr::mutate(
      x_facet = paste(
        x,
        facet,
        sep = "_"
      )) %>%
    dplyr::mutate(
      x_facet_count = dplyr::n_distinct(
        x_facet
      )) %>%
    dplyr::mutate(
      height_params = purrr::pmap(
        list(
          "figure_type_id" = figure_type_id,
          "facet_count" = facet_count,
          "x_facet_count" = x_facet_count
        ),
        list
      )) %>%
    dplyr::mutate(
      figure_height = purrr::map_dbl(
        height_params,
        RUBer::get_figure_height
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -facet_count,
      -x_facet,
      -x_facet_count,
      -height_params
    )

  return(df)
}

#' Add column \code{file_name} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{file_name}
#' @export
#'
#' @examples
#' add_file_name(df)
add_file_name <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_id,
      report_nr,
      subject_txt,
      subject_area_txt,
      is_reference_group
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      file_name_params = purrr::pmap(
        list(
          "report_id" = report_id,
          "report_nr" = report_nr,
          "subject_txt" = subject_txt,
          "subject_area_txt" = subject_area_txt
        ),
        list
      )
    ) %>%
    dplyr::mutate(
      file_name =
        dplyr::if_else(
          is_reference_group,
          NA_character_,
          purrr::map_chr(
            file_name_params,
            RUBer::get_file_name
          )
        )
    ) %>%
    dplyr::select(
      -file_name_params
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(
      cols = c(data)
    ) %>%
    tidyr::fill(
      file_name
    )

  return(df)
}

#' Get formula for calculating position of value labels
#'
#' @param label_var The name of the variabler requiring value labels
#' @inheritParams add_label_position
#'
#' @return A defused expression for caluclating the position of the y-label
#' @export
#'
#' @examples
#' get_label_formula(y_var = cyl, label_reverse = TRUE, is_percentage = FALSE)
get_label_formula <- function(label_var,
                              is_percentage = FALSE) {
  label_var <- rlang::enquo(label_var)

  if(!is_percentage) {
    label_formula <- rlang::expr(
      sum(!!label_var) - cumsum(!!label_var) + !!label_var / 2
    )
  } else if(is_percentage)  {
    label_formula <- rlang::expr(
      (sum(!!label_var) - cumsum(!!label_var) + !!label_var / 2) / sum(!!label_var)
    )
  }

  return(label_formula)
}


#' Add centered y-coordinates for filtered data labels of figure types 1 and 2
#'
#' Calculates the y-coordinates for stacked bar charts, centered for
#'    each group. The default function does not work if some labels are
#'    filtered.
#'
#' @inheritParams rub_plot_type_2
#' @param is_percentage Optional boolean indicating whether the value label is
#'     expressed in absolute numbers or as a percentage, defaults to false
#'
#' @return Data frame with additional column "label_" + the name of the
#'    y-coordinate variable
#' @export
#'
#' @examples
#' add_label_position(df, x_var, y_var, fill)
add_label_position <- function(df, x_var,
                               y_var, facet_var,
                               fill_var, filter_cutoff = 0.04,
                               fill_reverse = FALSE, is_percentage = FALSE) {

  has_facet <- !rlang::quo_is_null(
    rlang::enquo(
      facet_var
    )
  )

  # Group vars with and without fill_var for different summary statistics
  # Caution: Order of grouping vars matters for dplyr::summarise!
  if(!has_facet)  {
    group_vars <- dplyr::vars(
      rlang::ensym(x_var),
      rlang::ensym(fill_var)
      )
    } else if(has_facet) {
    group_vars <- dplyr::vars(
      rlang::ensym(x_var),
      rlang::ensym(facet_var),
      rlang::ensym(fill_var)
    )
  }

  y_var <- rlang::enquo(y_var)

  label_name <- paste0(
    "label_",
    rlang::as_label(y_var)
  )

  label_formula <- get_label_formula(
    label_var = {{y_var}},
    is_percentage = is_percentage
  )

  # y formula
  if(is_percentage) {
    y_formula <- rlang::expr(
      !!y_var / sum(!!y_var)
    )
  } else  {
    y_formula <- rlang::expr(
      !!y_var
    )
  }

  df_label <- df %>%
    dplyr::group_by_at(
      group_vars
    ) %>%
    dplyr::summarise(
      !!y_var := sum(!!y_var)
    ) %>%
    dplyr::mutate(
      # sum_var = sum(!!y_var),
      # cumsum_var = cumsum(!!y_var),
      # half_var_var = !!y_var / 2,
      !!label_name := eval(label_formula)
    )

  if(is_percentage) {
      df_label <- df_label %>%
        dplyr::ungroup(
          {{fill_var}}
        ) %>%
        dplyr::mutate(
          !!y_var := eval(y_formula)
        )
  }

  df_label_filtered <- df_label %>%
    dplyr::filter(
      round((!!y_var / sum(!!y_var)), digits = 2) >= filter_cutoff
    )

  return(df_label_filtered)
}

#' Adds column \code{report_author} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{report_author}
#' @export
#'
#' @examples
#' add_report_author(df)
add_report_author <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_nr,
      report_id,
      faculty_txt,
      subject_area_txt,
      subject_txt,
      is_reference_group
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      author_params = purrr::pmap(
        list(
          "report_id" = report_id,
          "faculty_txt" = faculty_txt,
          "subject_area_txt" = subject_area_txt,
          "subject_txt" = subject_txt
        ),
        list
      )) %>%
    dplyr::mutate(
      report_author = dplyr::if_else(
        is_reference_group,
        NA_character_,
        purrr::map_chr(
          author_params,
          RUBer::get_author
        ))) %>%
    dplyr::select(
      -author_params
    ) %>%
    tidyr::unnest(
      cols = c(data)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::fill(
      report_author
    )

  return(df)
  # More condensed version, less performant
  # df <- df %>%
  #   dplyr::mutate(
  #     author_params = purrr::pmap(
  #       list(
  #         "report_id" = report_id,
  #         "faculty_txt" = faculty_txt,
  #         "subject_area_txt" = subject_area_txt,
  #         "subject_txt" = subject_txt
  #       ),
  #       list
  #     )
  #   ) %>%
  #   dplyr::mutate(
  #     report_author = dplyr::if_else(
  #       is_reference_group,
  #       NA_character_,
  #       purrr::map_chr(
  #         author_params,
  #         RUBer::get_author
  #       )
  #     )
  #   ) %>%
  #   dplyr::select(
  #     -author_params
  #   ) %>%
  #   tidyr::fill(
  #     report_author
  #   )
}

#' Adds column \code{report_title} to data frame
#'
#' @param df
#'
#' @return Data frame with additional column \code{report_title}
#' @export
#'
#' @examples
#' add_report_title(df)
add_report_title <- function(df)  {
  df <- df %>%
    dplyr::mutate(
      report_title = purrr::map_chr(
        report_nr,
        RUBer::get_title
      ))

  return(df)
}
