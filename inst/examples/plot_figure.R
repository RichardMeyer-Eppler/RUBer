
RUBer::df_example %>%
  dplyr::filter(
    .data[["report_nr"]] == 6L,
    .data[["figure_nr"]] == 1L
  ) %>%
  RUBer::plot_figure(
    font_family = "sans"
  )
