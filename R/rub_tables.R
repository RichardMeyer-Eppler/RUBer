#' Get formatted flextable of cases
#'
#' @param df Data frame with columns Befragung_Typ_DTXT, Studienfachzaehler, Studiengang
#' @param label Label for the first column
#'
#' @return Formatted Flextable
#' @export
#'
#' @examples
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
