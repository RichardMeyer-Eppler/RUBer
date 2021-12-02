#' Get formatted flextable of cases
#'
#' @param df Data frame with columns Befragung_Typ_DTXT, Studienfachzaehler, Studiengang
#'
#' @return Formatted Flextable
#' @export
#'
#' @examples
rub_table_stg <- function(
  df
) {
  gtsummary::tbl_strata(
    data = df,
    strata = c(
      befragung_typ_dtxt
    ),
    .tbl_fun =
      ~ .x %>%
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
      gtsummary::modify_header(label ~ "") %>%
      gtsummary::modify_header(
        gtsummary::all_stat_cols() ~ "**{level}**\nN =  {n} ({style_percent(p)}%)"
      ) %>%
      gtsummary::add_overall(
        last = TRUE,
        col_label = "**Gesamt**\nN = {N}"
      )
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
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      i = 2,
      align = "right",
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
    )
}
