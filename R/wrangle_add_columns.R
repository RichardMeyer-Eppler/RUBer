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
        RUB::get_figure_caption
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
        RUB::get_figure_height
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
            RUB::get_file_name
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

#' Add centered y-coordinates for filtered data labels of vertical stacked bar
#' charts
#'
#' Calculates the y-coordinates for vertical stacked bar charts, centered for
#'    each group. The default function does not work if some labels are
#'    filtered.
#' @param df Data frame
#' @param x x-coordinate
#' @param y y-coordinate
#'
#' @return Data frame with additional column "label_" + the name of the
#'    y-coordinate variable
#' @export
#'
#' @examples
#' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- get_label_position(df, x, y)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
add_label_position_typ_1 <- function(df, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  label_name <- paste0(
    "label_",
    rlang::quo_name(y)
  )

  df <- df %>%
    dplyr::group_by(
      !!x
    ) %>%
    dplyr::mutate(
      !!label_name := sum(!!y) - cumsum(!!y) + !!y / 2
    )

  return(df)
}

#' Add centered y-coordinates for filtered data labels of vertical stacked bar
#' charts that are scaled to 100\%
#'
#' Calculates the y-coordinates for vertical stacked bar charts, centered for
#'    each group. The default function does not work if some labels are
#'    filtered.
#' @param df Data frame
#' @param x x-coordinate
#' @param y y-coordinate
#' @param facet facet variable
#'
#' @return Data frame with additional column "label_" + the name of the
#'    y-coordinate variable
#' @export
#'
#' @examples
#' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- get_label_position(df, x, y)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
add_label_position_typ_2 <- function(df, x, y, facet) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  label_name <- paste0(
    "label_",
    rlang::quo_name(y)
  )

  df <- df %>%
    dplyr::group_by(
      !!x,
      !!facet
    ) %>%
    dplyr::mutate(
      !!label_name := 1 - sum(!!y) + cumsum(!!y) - !!y / 2
    )

  return(df)
}

#' Add centered y-coordinates for filtered data labels of horizontal stacked bar
#' charts that are scaled to 100\%
#'
#' Calculates the y-coordinates for horizontal stacked bar charts, centered for
#'    each group. The default function does not work if some labels are
#'    filtered.
#' @param df Data frame
#' @param x x-coordinate
#' @param y y-coordinate
#' @param facet facet variable
#' @param group grouping variable
#' @param fill_reverse Boolean, whether the sort order of the grouping variable
#'    needs to be inverted
#'
#' @return Data frame with additional column "label_" + the name of the
#'    y-coordinate variable
#' @export
#'
#' @examples
#' #' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- get_label_position(df, x, y)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
add_label_position_typ_3 <- function(df, x, y, facet, group, fill_reverse = FALSE) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  group <- rlang::enquo(group)

  label_name <- paste0(
    "label_",
    rlang::quo_name(y)
  )

  df <- df %>%
    dplyr::group_by(
      !!x,
      !!facet,
      !!group
    ) %>%
    dplyr::mutate(
      !!label_name := if (!!fill_reverse) cumsum(!!y) - !!y / 2 else 1 - cumsum(!!y) + !!y / 2)
  return(df)
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
          RUB::get_author
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
  #         RUB::get_author
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
        RUB::get_title
      ))

  return(df)
}
