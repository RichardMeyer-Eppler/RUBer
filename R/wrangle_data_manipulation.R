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

#' Fixes data errors in report numbers 42, 43, 61 and 62.
#'
#' @param df
#'
#' @return Data frame with fixes to the columns \code{faculty_txt},
#'    \code{report_nr}, and \code{figure_sort}
#' @export
#'
#' @examples
#' fix_data_errors(df)
fix_data_errors <- function(df) {
  df <- df %>%
    dplyr::group_by(
      report_nr
    ) %>%
    dplyr::mutate(
      faculty_txt = dplyr::if_else(
        report_nr == 61,
        "Medizinische Fakultät",
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

#' Get function calls and parameters for use of purrr::map
#'
#' @param df Dataframe bzw. Tibble
#'
#' @return Dataframe bzw. Tibble with chunk_label and function_param columns
#' @export
#'
#' @examples
#' get_map_paramters(df)
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
      chunk_label = figure_count
    ) %>%
    dplyr::mutate(
      function_parameters = paste0(
        "(df, figure_count = ",
        figure_count,
        ")"
      )) %>%
    dplyr::select(
      -figure_count
    )

  return(map_parameters)
}

#' Filtere Datenlabels anhand eines Schwellwerts
#'
#' Filtert in gestapelten Säulendiagrammen die Datenlabels.).
#' @param df Dataframe bzw. Tibble
#' @param x Name der Variable, die als x-Koordinate verwendet wird
#' @param y Name der Variable, die als y-Koordinate verwendet wird
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (z.B. 0.05)
#'
#' @return Gefilterter Dataframe bzw. Tibble
#' @export
#'
#' @examples
#' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- filter_label_y(x, y, fill, y_label, filter_cutoff)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
filter_label <- function(df, x, y, filter_cutoff = 0.04) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  filter_cutoff <- rlang::enquo(filter_cutoff)

  df <- df %>%
    dplyr::group_by(!!x) %>%
    dplyr::filter((!!y / sum(!!y) > !!filter_cutoff) == TRUE)

  return(df)
}

#' Filtere Datenlabels anhand eines Schwellwerts
#'
#' Filtert in gestapelten Säulendiagrammen die Datenlabels.).
#' @param df Dataframe bzw. Tibble
#' @param x Name der Variable, die als x-Koordinate verwendet wird
#' @param y Name der Variable, die als y-Koordinate verwendet wird
#' @param facet Name der Variable, die die Abbildung in Facets aufteilt
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (z.B. 0.05)
#'
#' @return Gefilterter Dataframe bzw. Tibble
#' @export
#'
#' @examples
#' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- filter_label_y(x, y, fill, y_label, filter_cutoff)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
filter_label_typ_2 <- function(df, x, y, facet, filter_cutoff = 0.04) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  filter_cutoff <- rlang::enquo(filter_cutoff)

  df <- df %>%
    dplyr::group_by(!!x, !!facet) %>%
    dplyr::filter((!!y / sum(!!y) > !!filter_cutoff) == TRUE)

  return(df)
}

#' Filtere Datenlabels anhand eines Schwellwerts
#'
#' Filtert in gestapelten, rotierten Säulendiagrammen die Datenlabels.
#' @param df Dataframe bzw. Tibble
#' @param y Name der Variable, die als y-Koordinate verwendet wird
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (z.B. 0.05)
#'
#' @return Gefilterter Dataframe bzw. Tibble
#' @export
#'
#' @examples
#' x <- c("WiSe 13/14", "WiSe 13/14", "WiSe 13/14", "WiSe 13/14")
#' y <- c(1989, 58, 163, 470)
#' fill <- c("Bachelor 2-Fächer", "Master 1-Fach", "Master 2-Fächer", "Master of Education")
#' filter_cutoff <- 0.05
#' df <- filter_label_y(x, y, fill, y_label, filter_cutoff)
#' ggplot2::ggplot() +
#'   ggplot2::geom_bar(aes(x = x, y = y, fill = fill), stat = "identity") +
#'   ggplot2::geom_label(data = df, aes(x = x, y = y_label, group = fill, label = y))
filter_label_typ_3 <- function(df, y, filter_cutoff = 0.04) {
  y <- rlang::enquo(y)
  filter_cutoff <- rlang::enquo(filter_cutoff)

  df <- df %>%
    dplyr::filter(!! y > !!filter_cutoff)

  return(df)
}

#' Generiert für die Abbildungstypen 1-4 Variablen für die Ploterstellung
#'
#' @param df Dataframe bzw. Tibble
#'
#' @return Dataframe bzw. Tibble mit den zusätzlichen Spalten x, y, fill, fill_label, fill_reverse, facet und group.
#' @export
#'
#' @examples
#' mutate_plot_variables(df_filtered)
mutate_plot_variables <- function(df) {
  df <- df %>%
    dplyr::mutate(x = dplyr::case_when(
      figure_type_id %in% c(1, 4) ~ time,
      figure_type_id == 2 ~ variable_txt,
      figure_type_id == 3 ~ axis_label
    )) %>%
    dplyr::mutate(y = dplyr::case_when(
      figure_type_id %in% c(1, 4) ~ as.double(value_n_total),
      figure_type_id %in% c(2, 3) ~ as.double(value_percentage)
    )) %>%
    dplyr::mutate(y_label = dplyr::case_when(
      figure_type_id %in% c(1, 4) ~ axis_label,
    )) %>%
    dplyr::mutate(fill = dplyr::case_when(
      figure_type_id == 1 ~ degree_sort,
      figure_type_id %in% c(2, 3) ~ value_id
    )) %>%
    dplyr::mutate(fill_label = dplyr::case_when(
      figure_type_id == 1 ~ degree_txt,
      figure_type_id %in% c(2, 3) ~ value_txt
    )) %>%
    dplyr::mutate(fill_reverse = dplyr::case_when(
      is.na(scale_invert_flag) ~ FALSE,
      TRUE ~ scale_invert_flag
    )) %>%
    dplyr::mutate(facet = dplyr::case_when(
      figure_type_id == 2 ~ axis_label,
      figure_type_id == 3 ~ variable_txt
    )) %>%
    dplyr::mutate(group = dplyr::case_when(
      figure_type_id %in% c(3, 4) ~ degree_id
    )) %>%
    dplyr::mutate(group_label = dplyr::case_when(
      figure_type_id == 4 ~ degree_txt
    )) %>%
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
#'
#' @return Original data frame with new data for figure_id 4
#' @export
#'
#' @examples
#' replace_cohort_data(df, df_orga)
replace_cohort_data <- function(df, df_orga) {
  df <- dplyr::filter(
    df,
    figure_id != 4
  )

  df <- dplyr::union(
    df,
    RUB::get_cohort_data(
      df_orga
    )
  )

  df <- RUB::sort_report(
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
#' read_cohort_data()
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
#' @return Data frame with correct cohort analysis data
#' @export
#'
#' @examples
#' get_cohort_data(df_orga)
get_cohort_data <- function(df_orga)  {
  df_cohort <- RUB::read_cohort_data()
  df_cohort <- RUB::add_fgr_nrwbund_ltxt(df_cohort, df_orga)
  df_cohort <- RUB::translate_df(df_cohort)
  df_cohort_fg <- RUB::generate_cohort_data_2018_FG(df_cohort)
  df_cohort <- dplyr::bind_rows(
    df_cohort,
    df_cohort_fg
  )
  df_cohort <- RUB::update_cohort_axis_label(df_cohort)
  df_cohort <- RUB::sort_report(df_cohort)

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
#' update_cohort_axis_label(df)
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
#' @param df_cohort
#'
#' @return Data frame with cohort data for report_id "2018_FG"
#' @export
#'
#' @examples
#' generate_cohort_data_2018_FG(df_cohort)
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

#' Erzeuge Faktoren für die Abbildungserstellung
#'
#' @param df Dataframe bzw. Tibble
#'
#' @return Dataframe bzw. Tibble mit den Faktoren x, fill, fill_label und group
#' @export
#'
#' @examples
#' set_factors(df)
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

#' Split up data for large figures
#'
#' This checks whether the total number of rows for a figures exceeds a limit.
#' If it does, the variables of the figure will be split in pieces by assigning
#' an additional figure_sort_key. The size of a figure is calculated by the
#' number of unique x-Axis + Facet variables. The function currently creates
#' problems by doubling headings.
#'
#' @param df Data frame
#' @param max_row_limit The maximum number of rows for each figure sort key
#'
#' @return Data frame with newly assigned figure sort keys
#' @export
#'
#' @examples
#' split_up_big_figures(df)
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
      row_number = row_number(),
      new_number_of_figures = trunc(figure_row_total / max_row_limit) + 1,
      divisor = figure_var_total / new_number_of_figures,
      figure_sort_add = (ceiling(row_number / divisor) - 1)
    ) %>%
    dplyr::group_by(
      figure_sort_add,
      add = TRUE
    ) %>%
    dplyr::mutate(
      figure_sort_add_cond = dplyr::if_else(
        row_number == min(row_number) &
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
