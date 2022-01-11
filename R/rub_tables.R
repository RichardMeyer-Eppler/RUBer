#' Get formatted flextable of student cases
#'
#' @param df Data frame with columns `studiengang`, `studienfachzaehler`, `faelle`
#' @param label Label for the first column
#'
#' @return Formatted Flextable
#' @export
#'
#' @examples
#' RUBer::rub_table_stg(
#'   df = tibble::tribble(
#'     ~studiengang, ~studienfachzaehler, ~faelle,
#'     "Studiengang A", "1. Fach", 1,
#'     "Studiengang B", "2. Fach", 1
#'   ),
#'   label = "Beispieltabelle"
#' )
rub_table_stg <- function(
  df,
  label
) {

  df %>%
    gtsummary::tbl_summary(
      by = studienfachzaehler,
      label = list(
        studiengang ~ "Studiengang"
      ),
      statistic = list(
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      ),
      percent = "row",
      include = studiengang
    ) %>%
    gtsummary::modify_header(
      update = list(
        label ~ label
      )
    ) %>%
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ "**{level}**\nN =  {n} ({style_percent(p)}%)"
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
  headings
) {

  grey_fill <- "#ECECEC"

  footnote_text <- "Fragebögen werden als gültig kategorisiert, wenn mindestens eine Frage beantwortet wurde."

  rows_heading <- df %>%
    dplyr::filter(
      studieneingang %in% headings
    ) %>%
    dplyr::pull(
      row_id
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      studieneingang %in% headings == FALSE
    ) %>%
    dplyr::pull(
      row_id
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
      bg = grey_fill,
      part = "body"
    ) %>%
    flextable::set_formatter(
      koepfe_rub = function(x) RUBer::rub_format_mixed(x),
      koepfe_bef = function(x) RUBer::rub_format_mixed(x),
      koepfe_rub_perc = function(x) RUBer::rub_format_percent(x),
      koepfe_bef_perc = function(x) RUBer::rub_format_percent(x)
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
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center"
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
    )

  return(ft)
}

#' Get formatted flextable of response rates for the Verlaufsbefragung
#'
#' @param df Data frame
#' @param typology Data frame with flextable typology
#' @param headings Character vectors of headings
#'
#' @return Formatted Flextable
#' @export
#'
#' @examples
rub_table_vb <- function(
  df,
  typology,
  headings
) {

  grey_fill <- "#ECECEC"

  footnote_text <- "Fragebögen werden als gültig kategorisiert, wenn mindestens eine Frage beantwortet wurde."

  rows_heading <- df %>%
    dplyr::filter(
      studienverlauf %in% headings
    ) %>%
    dplyr::pull(
      row_id
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      studienverlauf %in% headings == FALSE
    ) %>%
    dplyr::pull(
      row_id
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
      bg = grey_fill,
      part = "body"
    ) %>%
    flextable::set_formatter(
      koepfe_2fs_rub = function(x) RUBer::rub_format_mixed(x),
      koepfe_2fs_bef = function(x) RUBer::rub_format_mixed(x),
      koepfe_5fs_rub = function(x) RUBer::rub_format_mixed(x),
      koepfe_5fs_bef = function(x) RUBer::rub_format_mixed(x),
      koepfe_2fs_rub_perc = function(x) RUBer::rub_format_percent(x),
      koepfe_2fs_bef_perc = function(x) RUBer::rub_format_percent(x),
      koepfe_5fs_rub_perc = function(x) RUBer::rub_format_percent(x),
      koepfe_5fs_bef_perc = function(x) RUBer::rub_format_percent(x)
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
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center"
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
    )

  return(ft)
}

#' Get formatted flextable of response rates for the Absolvent:innenbefragung
#'
#' @param df Data frame
#' @param typology Data frame with flextable typology
#' @param headings Character vectors of headings
#'
#' @return Formatted Flextable
#' @export
#'
#' @examples
rub_table_ab <- function(
  df,
  typology,
  headings
) {

  grey_fill <- "#ECECEC"

  footnote_text <- "Fragebögen werden als gültig kategorisiert, wenn mindestens eine Frage beantwortet wurde. Bei der Absolvent:innenbefragung werden nach Abschluss der Befragung Studienfälle aus den Datensätzen gelöscht, wenn nur sehr wenige Fragen beantwortet wurden. Diese ausgeschlossenen Fälle sind in dieser Rücklauftabelle noch enthalten, obwohl sie bei den eigentlichen Auswertungen nicht mehr berücksichtigt wurden."

  rows_heading <- df %>%
    dplyr::filter(
      studienabschluss %in% headings
    ) %>%
    dplyr::pull(
      row_id
    )

  rows_no_heading <- df %>%
    dplyr::filter(
      studienabschluss %in% headings == FALSE
    ) %>%
    dplyr::pull(
      row_id
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
      bg = grey_fill,
      part = "body"
    ) %>%
    flextable::set_formatter(
      koepfe_rub = function(x) RUBer::rub_format_mixed(x),
      koepfe_bef = function(x) RUBer::rub_format_mixed(x),
      koepfe_rub_perc = function(x) RUBer::rub_format_percent(x),
      koepfe_bef_perc = function(x) RUBer::rub_format_percent(x)
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
      i = flextable::nrow_part(
        .,
        part = "body"
      ),
      j = 2:flextable::ncol_keys(
        .
      ),
      align = "center"
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
  grey_fill <- "#ECECEC"

  typology <- tibble::tribble(
    ~col_keys,                                      ~colB,                                              ~colA,
    "programm",                                     "Programm",                                         "Programm",
    "projekttitel",                                 "Projekttitel",                                     "Projekttitel",
    "antragsteller_innen_verantwortliche_personen", "Antragsteller*innen / Verantwortliche Personen",   "Antragsteller*innen / Verantwortliche Personen",
    "forderzeitraum_von",                           "Förderzeitraum",                                   "von",
    "forderzeitraum_bis",                           "Förderzeitraum",                                   "bis"
  )

  rows <- df %>%
    dplyr::filter(
      is_last_row
    ) %>%
    dplyr::pull(
      rn
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
      even_body = grey_fill,
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
#' @examples
rub_table_item <- function(
  df
) {

  typology <- tibble::tribble(
    ~col_keys,          ~colB,                  ~colA,
    "y",                "Abschluss",            "Abschluss",
    "figure_caption",   "Abbildung",            "Abbildung",
    "facet",            "Item",                 "Item",
    "mean",             "Mittelwert (μ)",       "Studiengang",
    "mean_fgr",         "Mittelwert (μ)",       "Fächergruppe",
    "distance",         "Abweichung",           "in SD (\u03C3)"
  )

  # https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference/10116#10116
  eq <- "\\color{#003560}{\\frac{|\\mu^{faechergruppe}-\\mu^{studiengang}|}{ \\sigma^{faechergruppe}}}"

  ft <- df %>%
    dplyr::select(
      y,
      figure_caption,
      facet,
      mean,
      mean_fgr,
      distance
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
        "* Die Formel zur Berechnung der Abweichung verwendet neben den ausgewiesenen Mittelwerten noch die Standardabweichung der Fächergruppe, die in der Tabelle aus Platzgründen nicht ausgegeben wird. Die exakte Berechnung ist: ",
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
#' @examples
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
      bg = RUBer::get_RUB_colors("blue")
    ) %>%
    flextable::color(
      color = "white"
    )

  return(ft)
}
