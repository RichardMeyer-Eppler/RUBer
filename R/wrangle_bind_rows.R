#' Bind rows for \code{report_id} "2018_FG" to data frame
#'
#' @param df Data frame
#'
#' @return Data frame with additional rows for \code{report_id} "2018_FG"
#' @export
#'
#' @examples
#' add_2018_fg(df)
bind_2018_fg <- function(df) {
  df <- dplyr::union(
    df,
    get_2018_fg(df)
  )
}

#' Get data frame with generated data for \code{report_id} "2018_FG"
#'
#' @param df Data frame
#'
#' @return Data frame with all data for \code{report_id} "2018_FG"
#' @export
#'
#' @examples
#' get_2018_fg(df)
get_2018_fg <- function(df) {
  pattern_sg <- c(
    "Humanmedizin / Gesundheitswissenschaften" = "Humanmedizin,\nGesundheitswissenschaften"
    ,"Mathematik, Naturwissenschaften" = "Mathematik, \nNaturwissenschaften"
    ,"Rechts-, Wirtschafts- und Sozialwissenschaften" = "Rechts-, Wirtschafts- und\nSozialwissenschaften"
  )
  pattern_var <- "\\([^()]+\\)" ## Inhalt in runden Klammern

  df <- df %>%
    dplyr::filter(
      is.na(
        report_nr
      )) %>%
    dplyr::filter(
      report_id != "2018_M_ED"
    ) %>%
    dplyr::filter(
      figure_id %in% c(1:58)
    ) %>%
    dplyr::filter(
      value_n_total != 0
    ) %>%
    dplyr::mutate(
      report_id = "2018_FG",
      report_nr = 64,
      figure_sort = dplyr::if_else(
        figure_id < 5,
        as.integer(
          figure_id
        ),
        as.integer(
          figure_id + 5
        )),
      subject_group_txt_tmp = stringr::str_replace_all(
        subject_group_txt,
        pattern_sg
      ),
      axis_label = as.character(
        glue::glue(
          "{subject_group_txt_tmp} (n={value_n_total})"
        ))) %>%
    dplyr::select(
      -subject_group_txt_tmp
    ) %>%
    dplyr::group_by(
      figure_id
    ) %>%
    dplyr::mutate(variable_txt = dplyr::if_else(
      figure_id %in% c(3, 5, 6),
      stringr::str_replace(
        variable_txt,
        pattern = pattern_var,
        replacement = as.character(
          stringr::str_glue(
            "({degree_txt})"
          ))),
      as.character(
        stringr::str_glue(
          "{variable_txt} ({degree_txt})"
        )))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  return(df)
}

#' Get left hand side of generated subject group data
#'
#' @param df
#'
#' @return Data frame with left hand side of generated subject group data
#' @export
#'
#' @examples
#' get_subject_group_lhs(df)
get_subject_group_lhs <- function(df) {

  subject_group_lhs <- df %>%
    dplyr::filter(
      !(figure_id %in% c(1,2,4)) &
        report_id %in% c(
          "2018_LE"
          ,"2018_STG"
          ,"2018_LE_530"
          ,"2018_LE_802"
        )) %>%
    dplyr::select(
      -value_percentage,
      -value_n,
      -value_n_total,
      -axis_label,
      -subject_id,
      -subject_txt,
      -subject_area_id,
      -subject_area_txt,
      -faculty_txt,
      -scale_invert_flag
    )

  return(subject_group_lhs)
}

#' Get right hand side of generated subject group data
#'
#' @param df
#'
#' @return Data frame with right hand side of generated subject group data
#' @export
#'
#' @examples
#' get_subject_group_rhs(df)
get_subject_group_rhs <- function(df) {
  subject_group_rhs <- df %>%
    dplyr::filter(
      report_id %in% c(
        "2018_LE"
        ,"2018_STG"
        ,"2018_LE_530"
        ,"2018_LE_802"
      ) &
        is.na(
          report_nr
        )) %>%
    dplyr::mutate(
      is_reference_group = TRUE
    ) %>%
    dplyr::select(
      -report_nr,
      -figure_sort,
      -figure_txt,
      -degree_group_id,
      -degree_group_txt,
      -variable_txt,
      -value_txt,
      -degree_sort,
      -degree_txt,
      -time,
      -question_txt,
      -figure_type_id,
      -source_caption,
      -figure_filter_flag,
      -subject_group_txt
    )

  return(subject_group_rhs)
}

#' Joins left hand and right hand side of subject group data
#'
#' @param df_lhs Left hand side of subject group data
#' @param df_rhs Right hand side of subject group data
#'
#' @return Joined data frame
#' @export
#'
#' @examples
#' join_subject_group(df_lhs, df_rhs)
join_subject_group <- function(df_lhs, df_rhs) {
  subject_group_df <- df_lhs %>%
    dplyr::left_join(
      df_rhs,
      by = c(
        "report_id",
        "figure_id",
        "degree_id",
        "variable_id",
        "value_id",
        "subject_group_id"
      ))

  return(subject_group_df)
}

#' Bind rows of subject group data to data frame
#'
#' @param df
#'
#' @return Data frame with subject group data for each report_nr. This means
#'    that the subject group data is repeated several times over. Binding the
#'    rows to each report_nr makes filtering much easier, though.
#' @export
#'
#' @examples
#' bind_subject_group(df)
bind_subject_group <- function(df) {
  lhs_df <- get_subject_group_lhs(df)
  rhs_df <- get_subject_group_rhs(df)
  joined_df <- join_subject_group(lhs_df, rhs_df)

  df <- df %>%
    dplyr::mutate(
      is_reference_group = FALSE
    ) %>%
    dplyr::bind_rows(
      joined_df
    ) %>%
    dplyr::filter(
      !is.na(
        report_nr
      )
    ) %>%
    RUBer::sort_report()

  return(df)
}
