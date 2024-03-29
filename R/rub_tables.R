#' Get formatted flextable of student cases
#'
#' @param df Data frame with columns `studiengang`, `studienfachzaehler`, `faelle`
#' @param label Label for the first column
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_stg.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_stg.R
rub_table_stg <- function(
  df,
  label
) {

  df %>%
    gtsummary::tbl_summary(
      by = .data[["studienfachzaehler"]],
      label = list(
        .data[["studiengang"]] ~ "Studiengang"
      ),
      statistic = list(
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      ),
      percent = "row",
      include = .data[["studiengang"]]
    ) %>%
    gtsummary::modify_header(
      update = list(
        label ~ label
      )
    ) %>%
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ "**{level}**\nN =  {style_number(n)} ({style_percent(p)}%)"
    ) %>%
    gtsummary::add_overall(
      last = TRUE,
      col_label = "**Gesamt**\nN = {N}"
    ) %>%
    gtsummary::as_flex_table() %>%
    RUBer::rub_style_flextable() %>%
    flextable::bold(
      i = 1,
      j = 1,
      part = "body"
    ) %>%
    flextable::align(
      i = 1,
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      i = 2:flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "right",
      part = "body"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "header"
    ) %>%
    flextable::width(
      j = 1,
      width = 3.8
    ) %>%
    flextable::width(
      j = 2:flextable::ncol_keys(
        .
      ),
      width = 1
    )
}

#' Get formatted flextable of response rates for the Eingangsbefragung
#'
#' @param df Data frame
#' @param typology Data frame with flextable typology
#' @param headings Character vectors of headings
#' @param padding Integer, padding in pts (points) passed to `flextable::padding()`
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_eb.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_eb.R
rub_table_eb <- function(
  df,
  typology,
  headings,
  padding = 3L
) {

  footnote_text <- "Frageb\u00F6gen werden als g\u00FCltig kategorisiert, wenn mindestens eine Frage beantwortet wurde."

  rows_heading <- df %>%
    dplyr::filter(
      .data[["studieneingang"]] %in% headings
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      .data[["studieneingang"]] %in% headings == FALSE
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  ft <- flextable::flextable(
    df,
    col_keys = c(
      "studieneingang",
      "koepfe_rub",
      "koepfe_rub_perc",
      "koepfe_bef",
      "koepfe_bef_perc"
    )
  ) %>%
    flextable::set_header_df(
      mapping = typology,
      key = "col_keys"
    ) %>%
    flextable::merge_h(
      part = "header"
    ) %>%
    flextable::merge_v(
      part = "header"
    ) %>%
    flextable::merge_h(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      part = "body"
    ) %>%
    flextable::footnote(
      i = 2,
      j = flextable::ncol_keys(
        .
      ) - 1,
      value = flextable::as_paragraph(
        footnote_text
      ),
      ref_symbols = "*",
      part = "header"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::bold(
      i = rows_heading,
      part = "body"
    ) %>%
    flextable::bg(
      i = rows_heading,
      bg = get_RUB_colors("lighter grey"),
      part = "body"
    ) %>%
    flextable::align(
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      i = 3,
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "right",
      part = "header"
    ) %>%
    flextable::align(
      i = rows_no_heading,
      j = "studieneingang",
      align = "right",
      part = "body"
    ) %>%
    flextable::align(
      i = 1:flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      part = "body",
      align = "right"
    ) %>%
    flextable::align(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      part ="body",
      align = "center"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "header"
    ) %>%
    flextable::width(
      j = 1,
      width = 2
    ) %>%
    flextable::width(
      j = 2:flextable::ncol_keys(
        .
      ),
      width = 1
    ) %>%
    flextable::padding(
      padding = padding
    )

  return(ft)
}

#' Get formatted flextable of response rates for the Verlaufsbefragung
#'
#' @inheritParams rub_table_eb
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_vb.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_vb.R
rub_table_vb <- function(
  df,
  typology,
  headings,
  padding = 3L
) {

  footnote_text <- "Frageb\u00F6gen werden als g\u00FCltig kategorisiert, wenn mindestens eine Frage beantwortet wurde."

  rows_heading <- df %>%
    dplyr::filter(
      .data[["studienverlauf"]] %in% headings
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      .data[["studienverlauf"]] %in% headings == FALSE
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  ft <- flextable::flextable(
    df,
    col_keys = c(
      "studienverlauf",
      "koepfe_2fs_rub",
      "koepfe_2fs_rub_perc",
      "koepfe_2fs_bef",
      "koepfe_2fs_bef_perc",
      "koepfe_5fs_rub",
      "koepfe_5fs_rub_perc",
      "koepfe_5fs_bef",
      "koepfe_5fs_bef_perc"
    )
  ) %>%
    flextable::set_header_df(
      mapping = typology,
      key = "col_keys"
    ) %>%
    flextable::merge_h(
      part = "header"
    ) %>%
    flextable::merge_v(
      part = "header"
    ) %>%
    flextable::merge_h(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      part = "body"
    ) %>%
    flextable::footnote(
      i = 3,
      j = flextable::ncol_keys(
        .
      ) - 1,
      value = flextable::as_paragraph(
        footnote_text
      ),
      ref_symbols = "*",
      part = "header"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::bold(
      i = rows_heading,
      part = "body"
    ) %>%
    flextable::bg(
      i = rows_heading,
      bg = get_RUB_colors("lighter grey"),
      part = "body"
    ) %>%
    flextable::align(
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      i = 3,
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "right",
      part = "header"
    ) %>%
    flextable::align(
      i = rows_no_heading,
      j = "studienverlauf",
      align = "right",
      part = "body"
    ) %>%
    flextable::align(
      i = 1:flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      part = "body",
      align = "right"
    ) %>%
    flextable::align(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "header"
    ) %>%
    flextable::width(
      j = 1,
      width = 2
    ) %>%
    flextable::width(
      j = 2:flextable::ncol_keys(
        .
      ),
      width = 0.5
    ) %>%
    flextable::padding(
      padding = padding
    )

  return(ft)
}

#' Get formatted flextable of response rates for the Absolvent:innenbefragung
#'
#' @inheritParams rub_table_eb
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_ab.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_ab.R
rub_table_ab <- function(
  df,
  typology,
  headings,
  padding = 3L
) {

  footnote_text <- "Frageb\u00F6gen werden als g\u00FCltig kategorisiert, wenn mindestens eine Frage beantwortet wurde. Bei der Absolvent:innenbefragung werden nach Abschluss der Befragung Studienf\u00E4lle aus den Datens\u00E4tzen gel\u00F6scht, wenn nur sehr wenige Fragen beantwortet wurden. Diese ausgeschlossenen F\u00E4lle sind in dieser R\u00FCcklauftabelle noch enthalten, obwohl sie bei den eigentlichen Auswertungen nicht mehr ber\u00FCcksichtigt wurden."

  rows_heading <- df %>%
    dplyr::filter(
      .data[["studienabschluss"]] %in% headings
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      .data[["studienabschluss"]] %in% headings == FALSE
    ) %>%
    dplyr::pull(
      .data[["row_id"]]
    )

  ft <- flextable::flextable(
    df,
    col_keys = c(
      "studienabschluss",
      "koepfe_rub",
      "koepfe_rub_perc",
      "koepfe_bef",
      "koepfe_bef_perc"
    )
  ) %>%
    flextable::set_header_df(
      mapping = typology,
      key = "col_keys"
    ) %>%
    flextable::merge_h(
      part = "header"
    ) %>%
    flextable::merge_v(
      part = "header"
    ) %>%
    flextable::merge_h(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      part = "body"
    ) %>%
    flextable::footnote(
      i = 2,
      j = flextable::ncol_keys(
        .
      ) - 1,
      value = flextable::as_paragraph(
        footnote_text
      ),
      ref_symbols = "*",
      part = "header"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::bold(
      i = rows_heading,
      part = "body"
    ) %>%
    flextable::bg(
      i = rows_heading,
      bg = get_RUB_colors("lighter grey"),
      part = "body"
    ) %>%
    flextable::align(
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      i = 3,
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "right",
      part = "header"
    ) %>%
    flextable::align(
      i = rows_no_heading,
      j = "studienabschluss",
      align = "right",
      part = "body"
    ) %>%
    flextable::align(
      i = 1:flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      part = "body",
      align = "right"
    ) %>%
    flextable::align(
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "header"
    ) %>%
    flextable::width(
      j = 1,
      width = 2
    ) %>%
    flextable::width(
      j = 2:flextable::ncol_keys(
        .
      ),
      width = 1
    ) %>%
    flextable::padding(
      padding = padding
    )

  return(ft)
}

#' Get formatted flextable of funded projects
#'
#' @param df Data frame
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_programs.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_programs.R
rub_table_programs <- function(
  df
) {

  typology <- tibble::tribble(
    ~col_keys,                                      ~colB,                                              ~colA,
    "programm",                                     "Programm",                                         "Programm",
    "projekttitel",                                 "Projekttitel",                                     "Projekttitel",
    "antragsteller_innen_verantwortliche_personen", "Antragsteller*innen / Verantwortliche Personen",   "Antragsteller*innen / Verantwortliche Personen",
    "forderzeitraum_von",                           "F\u00F6rderzeitraum",                              "von",
    "forderzeitraum_bis",                           "F\u00F6rderzeitraum",                              "bis"
  )

  rows <- df %>%
    dplyr::mutate(
      rn = dplyr::row_number()
    ) %>%
    dplyr::filter(
      .data[["is_last_row"]]
    ) %>%
    dplyr::pull(
      .data[["rn"]]
    )

  ft <- flextable::flextable(
    df,
    col_keys = names(df)[2:6]
  ) %>%
    flextable::set_header_df(
      mapping = typology,
      key = "col_keys"
    ) %>%
    flextable::merge_h(
      part = "header"
    ) %>%
    flextable::merge_v(
      part = "header"
    ) %>%
    flextable::merge_v(
      j = 2,
      part = "body"
    ) %>%
    flextable::colformat_datetime(
      fmt_date = "%d.%m.%Y"
    ) %>%
    flextable::theme_zebra(
      even_body = get_RUB_colors("lighter grey"),
      odd_body = "transparent"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::bg(
      j = 1,
      bg = "transparent",
      part = "body"
    ) %>%
    flextable::bold(
      j = 1,
      part ="body"
    ) %>%
    flextable::align(
      j = (
        flextable::ncol_keys(
          .
        ) - 1
      ):flextable::ncol_keys(
        .
      ),
      align = "left",
      part = "all"
    ) %>%
    flextable::align(
      i = 1,
      j = (
        flextable::ncol_keys(
          .
        ) - 1
      ):flextable::ncol_keys(
        .
      ),
      align = "center",
      part = "header"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "header"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "body"
    ) %>%
    flextable::width(
      j = 4,
      width = 1.8,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 5,
      widt = 1.8,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 1,
      width = 3.5,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 2,
      width = 5.45,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 3,
      width = 4.34,
      unit = "cm"
    ) %>%
    flextable::border_remove() %>%
    flextable::padding(
      i = rows,
      padding.bottom = 18,
      part = "body"
    )

  return(ft)
}

#' Get formatted flextable of items with largest deviation from comparison group
#'
#' @param df Data frame
#'
#' @return Formatted Flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_item.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_item.R
rub_table_item <- function(
  df
) {

  typology <- tibble::tribble(
    ~col_keys,          ~colB,                  ~colA,
    "y",                "Abschluss",            "Abschluss",
    "figure_caption",   "Abbildung",            "Abbildung",
    "facet",            "Item",                 "Item",
    "mean",             "Mittelwert (\u03BC)",  "Studiengang",
    "mean_fgr",         "Mittelwert (\u03BC)",  "F\u00E4chergruppe",
    "distance",         "Abweichung",           "in SD (\u03C3)"
  )

  # https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference/10116#10116
  eq <- "\\color{#003560}{\\frac{|\\mu^{faechergruppe}-\\mu^{studiengang}|}{ \\sigma^{faechergruppe}}}"

  ft <- df %>%
    dplyr::select(
      .data[["y"]],
      .data[["figure_caption"]],
      .data[["facet"]],
      .data[["mean"]],
      .data[["mean_fgr"]],
      .data[["distance"]]
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_df(
      mapping = typology,
      key = "col_keys"
    ) %>%
    flextable::merge_h(
      part = "header"
    ) %>%
    flextable::merge_v(
      part = "header"
    ) %>%
    flextable::theme_zebra(
      even_body = "#ECECEC",
      odd_body = "transparent"
    ) %>%
    flextable::footnote(
      i = 1,
      j = flextable::ncol_keys(
        .
      ),
      value = flextable::as_paragraph(
        ""
      ),
      ref_symbols = "*",
      part = "header"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::merge_v(
      j = 1
    ) %>%
    flextable::align(
      j = flextable::ncol_keys(
        .
      ) - 2,
      align = "center",
      part = "header"
    ) %>%
    flextable::valign(
      valign = "top",
      part = "all"
    ) %>%
    flextable::colformat_double(
      big.mark = ".",
      digits = 2,
      decimal.mark = ","
    ) %>%
    flextable::border_remove() %>%
    flextable::bg(
      j = 1,
      bg = "transparent",
      part = "body"
    ) %>%
    flextable::bold(
      j = 1,
      part ="body"
    ) %>%
    flextable::compose(
      value = flextable::as_paragraph(
        "* Die Formel zur Berechnung der Abweichung verwendet neben den ausgewiesenen Mittelwerten noch die Standardabweichung der F\u00E4chergruppe, die in der Tabelle aus Platzgr\u00FCnden nicht ausgegeben wird. Die exakte Berechnung ist: ",
        flextable::as_equation(
          x = eq
        ),
        "."
      ),
      part = "footer"
    ) %>%
    flextable::width(
      j = 1,
      width = 2,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 2,
      width = 3.6,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 3,
      width = 5.1,
      unit = "cm"
    ) %>%
    flextable::width(
      j = 4:6,
      width = 2.3,
      unit = "cm"
    )

  return(ft)

}

#' Get formatted flextable of metrics
#'
#' @param df Data frame
#'
#' @return Formatted flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_metrics.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_metrics.R
rub_table_metrics <- function(
  df
) {

  ft <- df %>%
    flextable::flextable() %>%
    flextable::colformat_image(
      j = "col1",
      i = 1,
      width = .5,
      height = .5
    ) %>%
    flextable::colformat_image(
      j = "col6",
      i = 2,
      width = .5,
      height = .5
    ) %>%
    flextable::colformat_image(
      j = "col1",
      i = 3,
      width = .5,
      height = .5
    ) %>%
    flextable::colformat_image(
      j = "col6",
      i = 4,
      width = .5,
      height = .5
    ) %>%
    flextable::colformat_image(
      j = "col1",
      i = 5,
      width = .5,
      height = .5
    ) %>%
    flextable::colformat_image(
      j = "col6",
      i = 6,
      width = .5,
      height = .5
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::delete_part(
      part = "header"
    ) %>%
    flextable::align(
      j = "col1",
      i = c(1, 3, 5),
      align = "right"
    ) %>%
    flextable::align(
      j = "col1",
      i = c(2),
      align = "right"
    ) %>%
    flextable::align(
      j = "col3",
      i = c(4),
      align = "right"
    ) %>%
    flextable::align(
      j = "col2",
      i = c(6),
      align = "right"
    ) %>%
    flextable::border_remove() %>%
    flextable::merge_h() %>%
    flextable::hrule(
      rule = "exact"
    ) %>%
    flextable::height_all(
      height = 3.5,
      unit = "cm",
      part = "body"
    ) %>%
    flextable::width(
      width = 2.85,
      unit = "cm"
    ) %>%
    flextable::fontsize(
      size = 14
    ) %>%
    flextable::bold() %>%
    flextable::bg(
      bg = get_RUB_colors("blue")
    ) %>%
    flextable::color(
      color = "white"
    )

  return(ft)
}

#' Get formatted flextable of included programs
#'
#' @param df Data frame
#'
#' @return Formatted flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_included_programs.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_included_programs.R
rub_table_included_programs <- function(
  df
) {

  df <- df %>%
    dplyr::mutate(
      row_nr = dplyr::row_number()
    )

  row_footnote_m_ed <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "M_ED"
    ) %>%
    dplyr::pull(
      .data[["row_nr"]]
    )

  row_footnote_fgr <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "FGR"
    ) %>%
    dplyr::pull(
      .data[["row_nr"]]
    )

  row_footnote_szma <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "SZMA"
    ) %>%
    dplyr::pull(
      .data[["row_nr"]]
    )

  report_nr_m_ed <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "M_ED"
    ) %>%
    dplyr::pull(
      .data[["report_nr"]]
    ) %>%
    unique(
      .
    )

  report_nr_fgr <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "FGR"
    ) %>%
    dplyr::pull(
      .data[["report_nr"]]
    ) %>%
    unique(
      .
    )

  report_nr_szma <- df %>%
    dplyr::filter(
      .data[["report_type_id"]] == "SZMA"
    ) %>%
    dplyr::pull(
      .data[["report_nr"]]
    ) %>%
    unique(
      .
    )

  df <- df %>%
    dplyr::select(
      .data[["row_nr"]],
      head1 = .data[["report_nr"]],
      head2 = .data[["subject_degree"]],
      head3 = .data[["subject_group"]]
    )

  footnote_text_m_ed <- glue::glue(
    "Im Datenreport Nr. {report_nr_m_ed} sind f\u00E4chergruppen\u00FCbergreifend alle Master of Education Studieng\u00E4nge ber\u00FCcksichtigt, weshalb der Datenreport mehrfach aufgef\u00FChrt wird."
  )

  footnote_text_fgr <- glue::glue(
    "Im Datenreport Nr. {report_nr_fgr} sind alle Studieng\u00E4nge je F\u00E4chergruppe ber\u00FCcksichtigt, weshalb der Datenreport mehrfach aufgef\u00FChrt wird."
  )

  footnote_text_szma<- glue::glue(
    "Im Datenreport Nr. {report_nr_szma} sind f\u00E4chergruppen\u00FCbergreifend alle am Servicezentrum Mathematik und Anwendungen beteiligten Studieng\u00E4nge ber\u00FCcksichtigt, weshalb der Datenreport mehrfach aufgef\u00FChrt wird."
  )

  ft <- df %>%
    flextable::flextable(
      col_keys = c(
        "head1",
        "head2",
        "head3"
      )
    ) %>%
    flextable::theme_zebra(
      even_body = "#ECECEC",
      odd_body = "transparent"
    ) %>%
    flextable::footnote(
      i = row_footnote_m_ed,
      value = flextable::as_paragraph(
        footnote_text_m_ed
      ),
      ref_symbols = "*",
      part = "body"
    ) %>%
    flextable::footnote(
      i = row_footnote_fgr,
      value = flextable::as_paragraph(
        footnote_text_fgr
      ),
      ref_symbols = "**",
      part = "body"
    ) %>%
    flextable::footnote(
      i = row_footnote_szma,
      value = flextable::as_paragraph(
        footnote_text_szma
      ),
      ref_symbols = "***",
      part = "body"
    ) %>%
    RUBer::rub_style_flextable() %>%
    flextable::set_header_labels(
      head1 = "Datenreport Nr.",
      head2 = "Fach-Abschluss-Kombinationen",
      head3 = "F\u00E4chergruppe"
    ) %>%
    flextable::align(
      j = "head1",
      align = "center",
      part = "body"
    ) %>%
    flextable::align(
      j = "head1",
      align = "center",
      part = "header"
    ) %>%
    flextable::width(
      j = "head1",
      width = 1
    ) %>%
    flextable::width(
      j = "head2",
      width = 3.63
    ) %>%
    flextable::width(
      j = "head3",
      width = 2.25
    )

  return(ft)
}

#' Get formatted flextable of excluded programs
#'
#' @param df Data frame
#'
#' @return Formatted flextable
#' @export
#'
#' @section Illustrations:
#'
#' \if{html}{\figure{rub_table_excluded_programs.png}{options: width=100\%}}
#'
#' @example inst/examples/rub_table_excluded_programs.R
rub_table_excluded_programs <- function(
  df
) {

  ft <- df %>%
    flextable::flextable(
      col_keys = c(
        "head1",
        "head2",
        "head3"
      )
    ) %>%
    flextable::set_header_labels(
      head1 = "Ausschlussgrund",
      head2 = "F\u00E4chergruppe",
      head3 = "Fach-Abschluss-Kombinationen"
    ) %>%
    flextable::width(
      j = "head1",
      width = 1.1
    ) %>%
    flextable::width(
      j = "head2",
      width = 2.15
    ) %>%
    flextable::width(
      j = "head3",
      width = 3.63
    ) %>%
    RUBer::rub_style_flextable(
      zebra = "even"
    )

 return(ft)
}
