#' Translate data frame columns from German to English
#'
#' @param df The original data set in German
#'
#' @return Translated data set
#' @export
#'
#' @examples
#' translate_df(df)
translate_df <- function(df)  {
  df <- df %>%
    dplyr::select(
      report_id = bericht_id,
      report_nr = bericht_nr,
      figure_id = abbildung_id,
      figure_sort = abbildung_sort_key,
      figure_txt = abbildung_txt,
      degree_group_id = abschluss_gruppe_id,
      degree_group_txt = abschluss_gruppe_txt,
      variable_id,
      variable_txt,
      value_id = wert_id,
      value_txt = wert_txt,
      value_percentage = anteil_wert,
      value_n = wert_abschl_n,
      value_n_total = abschl_n,
      axis_label = achse_label,
      subject_id = fach_rub_id,
      subject_txt = fach_rub_ltxt,
      subject_area_id = le_rub_id_1,
      subject_area_txt = le_rub_ltxt_1,
      subject_group_id = fgr_nrwbund_id,
      faculty_txt = fak_rub_ltxt_1,
      degree_id = abschluss_rub_id,
      degree_sort = abschluss_sort1,
      degree_txt = abschluss_rub_txt,
      time = zeit,
      question_txt = frage_ltxt,
      figure_type_id = abbildung_typ_id,
      source_caption = abb_quelle_txt,
      figure_filter_flag = abb_filter,
      scale_invert_flag = wert_skala_invertieren,
      subject_group_txt = fgr_nrwbund_ltxt
    )
}

#' Erzeuge Übersicht aller Berichte aus den Daten
#'
#' @param df Dataframe bzw. Tibble mit den Berichtsdaten
#'
#' @return Dataframe bzw. Tibble mit Berichtsübersicht
#' @export
#'
#' @examples
#' orga <- generate_orga(df)
generate_orga <- function(df) {
  orga <- df %>%
    dplyr::distinct(
      report_nr,
      report_id,
      subject_id,
      subject_txt,
      degree_id,
      subject_area_id,
      subject_area_txt,
      subject_group_id
    ) %>%
    dplyr::select(
      report_nr,
      report_id,
      fach_id = subject_id,
      fach = subject_txt,
      abschluss_id = degree_id,
      lehreinheit_id = subject_area_id,
      lehreinheit = subject_area_txt,
      faechergruppe_id = subject_group_id
    ) %>%
    dplyr::arrange(
      report_nr,
      report_id,
      fach_id,
      abschluss_id,
      lehreinheit_id,
      faechergruppe_id
    )

  file_path <- here::here("data", "orga.rds")
  readr::write_rds(orga, file_path)
  orga <- readr::read_rds(file_path)

  return(orga)
}


#' Filtere Datensatz auf eine Berichtsnummer plus jeweils relevante Vergleichsgruppe
#'
#' @param df Dataframe bzw. Tibble mit den Berichtsdaten
#' @param report_nr Berichtsnummer
#'
#' @return Dataframe bzw. Tibble mit den gefilterten Berichtsdaten
#' @export
#'
#' @examples
#' filter_report(df, report_nr = 12)
filter_report <- function(df, report_nr) {
  filtered_df <- df %>%
    dplyr::filter(
      report_nr == {{ report_nr }} &
        !figure_filter_flag
    )

  return(filtered_df)
}

#' Add headings to report data frame
#'
#' @param df Data frame
#'
#' @return Data frame with four additional columns: "heading", "subheading",
#' "is_heading" and "is_subheading".
#' @export
#'
#' @examples
#' get_headings(df)
get_headings <- function(df) {
  headings <- readxl::read_xlsx(
    path = here::here("data", "headings.xlsx"),
    na = c("", "NA")
  ) %>%
    dplyr::select(
      report_id,
      figure_id,
      heading,
      subheading
    )

  headings_df <- df %>%
    dplyr::select(
      report_id,
      report_nr,
      figure_id,
      figure_count
    ) %>%
    dplyr::filter(
      !is.na(
        figure_count
      )
    ) %>%
    dplyr::left_join(
      headings, by = c(
        "report_id",
        "figure_id"
      )) %>%
    dplyr::group_by(
      report_nr,
      heading
    ) %>%
    dplyr::mutate(
      is_heading = dplyr::if_else(
        min(
          figure_count
        ) == figure_count,
        TRUE,
        FALSE
      )) %>%
    dplyr::group_by(
      report_nr,
      subheading
    ) %>%
    dplyr::mutate(
      is_subheading = dplyr::if_else(
        min(
          figure_count
        ) == figure_count &
          !is.na(
            subheading
          ),
        TRUE,
        FALSE
      )) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df <- dplyr::left_join(
    df, headings_df, by = c(
      "report_id",
      "report_nr",
      "figure_id",
      "figure_count"
    )
  )

  return(df)
}

#' Returns all unique values for a specified column in a data frame
#'
#' @param df Data frame
#' @param column Column name
#'
#' @return Vector with unique values
#' @export
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

#' Format flextable mixed type columns (integer, percentages, NAs)
#'
#' @param x Vector
#'
#' @return Formatted vector
#' @export
#'
#' @examples
#' x <- c("2500", "0.29", NA)
#' rub_format_mixed(x)
rub_format_mixed <- function(x) {
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      x[i] <- ""
    }
    else if (suppressWarnings(
      !is.na(
        as.double(x[i])
      )
    )) {
      if ((as.double(x[i])) %% 1 == 0) {
        x[i] <- as.character(
          format(as.integer(x[i]),
                 big.mark = ".",
                 decimal.mark = ","
          )
        )
      }
      else {
        x[i] <- as.character(
          sprintf(
            "%.01f%%",
            as.double(x[i]) * 100
          )
        )
      }
    }
  }

  return(x)
}

#' Format flextable columns with percentages (percentages, NAs)
#'
#' @param x Vector
#'
#' @return Formatted vector
#' @export
#'
#' @examples
#' x <- c("0.29", NA)
#' rub_format_percent(x)
rub_format_percent <- function(x) {
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      x[i] <- ""
    }

    else if (suppressWarnings(
      !is.na(
        as.double(x[i])
      )
    )) {
      x[i] <- as.character(
        sprintf(
          "%.00f%%",
          as.double(x[i]) * 100
        )
      )
    }
  }

  return(x)
}

#' Sorts the report data frame
#'
#' Sorts report_nr, figure_sort, subject_id, subject_area_id, degree_sort,
#' time, variable_id, subject_group_id, value_id
#'
#' @param df Data frame with report data
#'
#' @return Sorted data frame
#' @export
#'
#' @examples
#' sort_report(df)
sort_report <- function(df) {
  df_sorted <- df %>%
    dplyr::arrange(
      report_nr,
      figure_sort,
      subject_id,
      subject_area_id,
      degree_sort,
      time,
      variable_id,
      subject_group_id,
      value_id
    )

  return(df_sorted)
}
