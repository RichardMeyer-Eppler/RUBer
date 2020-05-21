#' Fixes data errors in report numbers 42, 43, 61 and 62.
#'
#' @param df Data frame
#'
#' @return Data frame with fixes to the columns \code{faculty_txt},
#'    \code{report_nr}, and \code{figure_sort}
#' @export
#'
#' @examples
#' \dontrun{
#' fix_data_errors(df)
#' }
fix_data_errors <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_nr
    ) %>%
    dplyr::mutate(
      faculty_txt = dplyr::if_else(
        report_nr == 61,
        "Medizinische Fakult\u00e4t",
        faculty_txt
      )
    ) %>%
    dplyr::group_by(
      subject_area_id
    ) %>%
    dplyr::mutate(
      report_nr = dplyr::if_else(
        subject_area_id != 504 |
          is.na(subject_area_id),
        report_nr,
        42
      )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      figure_sort = dplyr::case_when(
        report_nr == 62 & figure_id == 4 & subject_area_id == "NAT" ~ 4,
        report_nr == 62 & figure_id == 4 & subject_area_id == "PHI" ~ 5,
        report_nr == 62 & figure_id == 4 & subject_area_id == "SGE" ~ 6,
        report_nr == 62 & figure_id == 4 & subject_area_id == "SOZ" ~ 7,
        report_nr == 62 & figure_id == 4 & subject_area_id == "SPO" ~ 8,
        report_nr == 62 & figure_id > 4 ~ figure_sort + 4,
        TRUE ~ figure_sort
      )
    )

  return(df)
}

#' Get function calls and parameters for use of purrr::map
#'
#' @param df Data frame
#'
#' @return Data frame with chunk_label and function_param columns
#' @export
#'
#' @examples
#' \dontrun{
#' get_map_paramters(df)
#' }
get_map_parameters <- function(df) {
  map_parameters <- df %>%
    dplyr::filter(
      !is.na(
        figure_count
        )
      ) %>%
    dplyr::distinct(
      figure_count,
      ) %>%
    dplyr::mutate(
      chunk_label = figure_count,
      function_parameters = paste0(
        "(df, figure_count = ",
        figure_count,
        ")"
        )
      ) %>%
    dplyr::select(
      -figure_count
      )

  return(map_parameters)
}

#' Generiert für die Abbildungstypen 1-4 Variablen für die Ploterstellung
#'
#' @param df Data frame
#'
#' @return Data frame with additional columns \code{x}, \code{y}, \code{fill},
#'     \code{fill_label}, \code{fill_reverse}, \code{facet} und \code{group}.
#' @export
#'
#' @examples
#' \dontrun{
#' mutate_plot_variables(df_filtered)
#' }
mutate_plot_variables <- function(df) {
  df <- df %>%
    dplyr::mutate(
      x = dplyr::case_when(
        figure_type_id %in% c(1L, 4L) ~ time,
        figure_type_id == 2L ~ variable_txt,
        figure_type_id == 3L ~ as.character(value_percentage)
        ),
      y = dplyr::case_when(
        figure_type_id %in% c(1L, 4L) ~ as.character(value_n_total),
        figure_type_id == 2L ~ as.character(value_percentage),
        figure_type_id == 3L ~ axis_label
        ),
      y_label = dplyr::case_when(
        figure_type_id %in% c(1L, 4L) ~ axis_label,
        ),
      fill = dplyr::case_when(
        figure_type_id == 1L ~ degree_sort,
        figure_type_id %in% c(2L, 3L) ~ value_id
        ),
      fill_label = dplyr::case_when(
        figure_type_id == 1L ~ degree_txt,
        figure_type_id %in% c(2L, 3L) ~ value_txt
        ),
      fill_reverse = dplyr::case_when(
        is.na(scale_invert_flag) ~ FALSE,
        TRUE ~ scale_invert_flag
        ),
      facet = dplyr::case_when(
        figure_type_id == 2L ~ axis_label,
        figure_type_id == 3L ~ variable_txt
        ),
      group = dplyr::case_when(
        figure_type_id %in% c(3L, 4L) ~ degree_id
        ),
      group_label = dplyr::case_when(
        figure_type_id == 4L ~ degree_txt
        )
      ) %>%
    dplyr::select(
      report_nr,
      figure_count,
      x,
      y,
      y_label,
      fill,
      fill_label,
      fill_reverse,
      facet,
      group,
      group_label,
      source_caption,
      question_txt,
      figure_type_id,
      figure_caption,
      heading,
      subheading,
      is_heading,
      is_subheading,
      report_author,
      report_title,
      file_name,
      figure_filter_flag
    )

  return(df)
}

#' Replaces original cohort data with corrected cohort data.
#'
#' @param df Data frame
#' @param df_orga Data frame with organization hierarchies
#'
#' @return Original data frame with new data for figure_id 4
#' @export
#'
#' @examples
#' \dontrun{
#' replace_cohort_data(df, df_orga)
#' }
replace_cohort_data <- function(df, df_orga) {
  df <- dplyr::filter(
    df,
    figure_id != 4
  )

  df <- dplyr::union(
    df,
    get_cohort_data(
      df_orga
    )
  )

  df <- sort_report(
    df
  )

  return(df)
}

#' Reads csv file with cohort data
#'
#' @return Data frame with cohort data
#' @export
#'
#' @examples
#' \dontrun{
#' read_cohort_data()
#' }
read_cohort_data <- function()  {
  df_cohort <- tibble::as_tibble(
    readr::read_csv2(
      file = here::here(
        "data", "Kohortenanalyse_2018_korrigiert.csv"),
      col_types = readr::cols(
        bericht_id = readr::col_character(),
        bericht_nr = readr::col_double(),
        achse_label = readr::col_character(),
        fach_rub_id = readr::col_character(),
        le_rub_id_1 = readr::col_character(),
        fgr_nrwbund_id = readr::col_integer(),
        befragung_typ_id = readr::col_character(),
        abschluss_rub_id = readr::col_integer(),
        abschluss_rub_txt = readr::col_character(),
        abschluss_gruppe_id = readr::col_integer(),
        abschluss_gruppe_txt = readr::col_character(),
        abschluss_sort1 = readr::col_integer(),
        zeit = readr::col_character(),
        abbildung_id = readr::col_integer(),
        abbildung_sort_key = readr::col_integer(),
        abbildung_txt = readr::col_character(),
        frage_ltxt = readr::col_character(),
        variable_id = readr::col_character(),
        variable_txt = readr::col_character(),
        wert_id = readr::col_integer(),
        wert_txt = readr::col_character(),
        fach_rub_ltxt = readr::col_character(),
        le_rub_ltxt_1 = readr::col_character(),
        fak_rub_ltxt_1 = readr::col_character(),
        anteil_wert = readr::col_number(),
        wert_abschl_n = readr::col_number(),
        abschl_n = readr::col_integer(),
        n_filter_flag = readr::col_integer(),
        abb_filter = readr::col_logical(),
        abbildung_typ_id = readr::col_integer(),
        variablen_je_abbildung = readr::col_integer(),
        werte_je_abbildung = readr::col_integer(),
        wert_skala_invertieren = readr::col_logical(),
        abb_quelle_txt = readr::col_character()
      ),
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  )
  return(df_cohort)
}

#' Get cohort data, including generated cohort data for report_id 2018_FG
#'
#' @param df_orga Data frame with orga
#'
#' @return Data frame with corrected cohort analysis data
#' @export
#'
#' @examples
#' \dontrun{
#' get_cohort_data(df_orga)
#' }
get_cohort_data <- function(df_orga)  {
  df_cohort <- read_cohort_data()
  df_cohort <- add_fgr_nrwbund_ltxt(df_cohort, df_orga)
  df_cohort <- translate_df(df_cohort)
  df_cohort_fg <- generate_cohort_data_2018_FG(df_cohort)
  df_cohort <- dplyr::bind_rows(
    df_cohort,
    df_cohort_fg
  )
  df_cohort <- update_cohort_axis_label(df_cohort)
  df_cohort <- sort_report(df_cohort)

  return(df_cohort)
}

#' Update axis label for cohort data
#'
#' @param df Data frame
#'
#' @return Data frame with updated column "axis_label"
#' @export
#'
#' @examples
#' \dontrun{
#' update_cohort_axis_label(df)
#' }
update_cohort_axis_label <- function(df) {
  df <- df %>%
    dplyr::mutate(
      axis_label = dplyr::case_when(
        report_id != "2018_FG" ~ as.character(
          glue::glue(
            "{degree_txt}: {axis_label}"
          )),
        report_id == "2018_FG" ~ as.character(
          glue::glue(
            "{subject_group_txt} ({degree_txt}): {axis_label} (n={round(value_n_total_max)})"
          ))
      ))

  df <- dplyr::select(
    df,
    -value_n_total_max
  )

  return(df)
}

#' Generate cohort data on the level of subject degree groups
#'
#' @param df_cohort Data frame
#'
#' @return Data frame with cohort data for report_id "2018_FG"
#' @export
#'
#' @examples
#' \dontrun{
#' generate_cohort_data_2018_FG(df_cohort)
#' }
generate_cohort_data_2018_FG <- function(df_cohort) {

  ## Replaces content of round brackets
  pattern_axis <- "\\([^()]+\\)"
  ## Adds line brakes to long subject group names
  pattern_sg <- c(
    "Humanmedizin / Gesundheitswissenschaften" =
      "Humanmedizin,\nGesundheitswissenschaften",
    "Mathematik, Naturwissenschaften" =
      "Mathematik, \nNaturwissenschaften",
    "Rechts-, Wirtschafts- und Sozialwissenschaften" =
      "Rechts-, Wirtschafts- und\nSozialwissenschaften"
  )

  df_cohort <- df_cohort %>%
    dplyr::mutate(axis_label = stringr::str_trim(
      stringr::str_replace(
        axis_label,
        pattern = pattern_axis,
        replacement = ""
      ))) %>%
    dplyr::mutate(value_id = dplyr::if_else(
      value_txt == "Exmatrikulation (mit oder ohne Abschluss)",
      4L,
      value_id
    )) %>%
    dplyr::filter(
      report_id != "2018_M_ED"
    )

  value_n_total <- df_cohort %>%
    dplyr::group_by(
      axis_label,
      subject_group_id,
      degree_id,
      figure_id,
      variable_id
    ) %>%
    dplyr::summarise(
      value_n_total = sum(
        value_n
      ))

  value_n_total_max <- value_n_total %>%
    dplyr::group_by(
      subject_group_id,
      degree_id,
      figure_id,
    ) %>%
    dplyr::summarise(
      value_n_total_max = max(
        value_n_total
      ))

  df_cohort_fg <- df_cohort %>%
    dplyr::group_by(
      figure_id,
      figure_sort,
      figure_txt,
      degree_group_id,
      degree_group_txt,
      variable_id,
      variable_txt,
      value_id,
      value_txt,
      subject_group_id,
      degree_id,
      degree_sort,
      degree_txt,
      time,
      question_txt,
      figure_type_id,
      source_caption,
      scale_invert_flag,
      subject_group_txt
    ) %>%
    dplyr::summarise(
      value_n = sum(
        value_n
      )) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      value_n_total_max,
      by = c(
        "figure_id",
        "subject_group_id",
        "degree_id"
      )) %>%
    dplyr::left_join(
      value_n_total,
      by = c(
        "figure_id",
        "subject_group_id",
        "degree_id",
        "variable_id"
      )) %>%
    dplyr::mutate(
      value_percentage = value_n / value_n_total
    ) %>%
    dplyr::mutate(
      report_id = "2018_FG"
    ) %>%
    dplyr::mutate(
      report_nr = 64
    ) %>%
    dplyr::mutate(
      figure_filter_flag = FALSE
    ) %>%
    dplyr::mutate(
      subject_group_count = dplyr::case_when(
        subject_group_id == 1 ~ 0,
        subject_group_id == 2 ~ 1,
        subject_group_id == 3 ~ 2,
        subject_group_id == 4 ~ 3,
        subject_group_id == 8 ~ 4,
        subject_group_id == 9 ~ 5
      )) %>%
    dplyr::mutate(
      figure_sort = figure_sort + subject_group_count
    ) %>%
    dplyr::select(
      -subject_group_count
    )

  return(df_cohort_fg)
}

#' Creates factors for plotting the 2018 data
#'
#' @param df Data frame
#'
#' @return Data frame with the factors x, fill, fill_label and group
#' @export
#'
#' @examples
#' \dontrun{
#' set_factors(df)
#' }
set_factors <- function(df) {
  df$x <- factor(df$x, levels = unique(df$x))
  df$fill <- factor(df$fill, levels = unique(df$fill))
  df$fill_label <- factor(df$fill_label, levels = unique(df$fill_label))
  df$group <- factor(df$group, levels = unique(df$group))

  if(df[[1, "fill_reverse"]]) {
    df$fill <- forcats::fct_rev(df$fill)
    df$fill_label <- rev(df$fill_label)
    df$group <- rev(df$group)
  }

  if(df[[1, "figure_type_id"]] == 3)  {
    df$x <- forcats::fct_rev(df$x)
  }

  return(df)
}

#' Prepares var and var_label columns for plotting by ordering them and turning
#' them into factors
#'
#' @param df Data frame
#' @param var Required variable name for the discrete variable to be turned into
#'     a factor. This variable is sorted alphabetically to determine the order.
#' @param var_label Optional variable name for the discrete variable labels
#'     to be used instead of var. If var is a sort key, for instance, var_label
#'     can be used for the actual labels.
#' @param reverse Whether the order of the factor should be reverted, defaults
#'     to FALSE.
#'
#' @return Data frame with factor column.
#' @export
#'
#' @examples
#' \dontrun{
#' set_factor_var(df, var, TRUE)
#' }
set_factor_var <- function(df, var, var_label = NULL, reverse = FALSE)  {
  var_sym <- rlang::ensym(var)

  # Var is always used to determine the ordering, even if var_label will later
  # get displayed. This allows, for instance, to have sort_keys in var and the
  # associated labels in var_label.
  # Note that unlike base factor, forcats::as_factor does not alter ordering
  df_ordered <- dplyr::arrange(df, {{var}})

  var_label_quo <- rlang::enquo(var_label)
  is_null_var_label_quo <- rlang::quo_is_null(var_label_quo)

  if(!is_null_var_label_quo)  {
    var_label_sym <- rlang::ensym(var_label)

    # If var_label is not a factor yet, it will get converted to one
    is_factor_var_label <- is.factor(df_ordered[[var_label_sym]])
    if(!is_factor_var_label)  {
      df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_label_sym]])
    }
    # var_label effectively replaces var. We only needed var for the ordering.
    # Use of forcats::reorder is not possible, because it only works for numeric
    # and var may not be numeric.
    df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_label_sym]])
  }

  is_factor_var <- is.factor(df[[var_sym]])
  # If var is not yet a factor, it gets gets turned into a factor. Otherwise
  # no action required.
  if(!is_factor_var)  {
    df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_sym]])
  }

  if (reverse) {
    df_ordered[[var_sym]] <- forcats::fct_rev(df_ordered[[var_sym]])
  }

  return(df_ordered)
}

#' Split up data for large figures
#'
#' This checks whether the total number of rows for a figures exceeds a limit.
#' If it does, the variables of the figure will be split in pieces by assigning
#' an additional figure_sort_key. The size of a figure is calculated by the
#' number of unique x-axis + facet variables.
#'
#' @param df Data frame
#' @param max_row_limit The maximum number of rows for each figure sort key
#'
#' @return Data frame with newly assigned figure sort keys
#' @export
#'
#' @examples
#' \dontrun{
#' split_up_big_figures(df)
#' }
split_up_big_figures <- function(df, max_row_limit = 30)  {
  big_figures <- df %>%
    dplyr::mutate(
      figure_size = paste(
        figure_sort,
        figure_txt,
        variable_id,
        axis_label,
        degree_group_id,
        sep = "_"
      )
    ) %>%
    dplyr::distinct(
      report_nr,
      figure_sort,
      degree_group_id,
      variable_id,
      figure_size
    ) %>%
    dplyr::add_count(
      report_nr,
      figure_sort,
      degree_group_id,
      name = "figure_row_total"
    ) %>%
    dplyr::distinct(
      report_nr,
      figure_sort,
      degree_group_id,
      variable_id,
      figure_row_total
    ) %>%
    dplyr::add_count(
      report_nr,
      figure_sort,
      degree_group_id,
      name = "figure_var_total"
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_sort,
      degree_group_id
    ) %>%
    dplyr::arrange(
      report_nr,
      figure_sort,
      degree_group_id,
      variable_id
    ) %>%
    dplyr::mutate(
      row_number_tmp = dplyr::row_number(),
      new_number_of_figures = trunc(figure_row_total / max_row_limit) + 1,
      divisor = figure_var_total / new_number_of_figures,
      figure_sort_add = (ceiling(row_number_tmp / divisor) - 1)
    ) %>%
    dplyr::group_by(
      figure_sort_add,
      .add = TRUE
    ) %>%
    dplyr::mutate(
      figure_sort_add_cond = dplyr::if_else(
        row_number_tmp == min(row_number_tmp) &
          figure_sort_add != 0,
        1,
        0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      figure_sort_cumsum = cumsum(
        figure_sort_add_cond
      ),
      figure_sort_new = figure_sort + figure_sort_cumsum
    ) %>%
    dplyr::select(
      report_nr,
      figure_sort,
      degree_group_id,
      variable_id,
      figure_sort_new
    )

  df <- df %>%
    dplyr::left_join(
      big_figures,
      by = c(
        "report_nr",
        "figure_sort",
        "degree_group_id",
        "variable_id"
      )
    ) %>%
    dplyr::select(
      -figure_sort
    ) %>%
    dplyr::rename(
      figure_sort = figure_sort_new
    )

  return(df)
}
