df_example %>%
  dplyr::filter(
    .data[["report_nr"]] == 6L,
    .data[["figure_count"]] == 1L
  ) %>%
  RUBer::plot_figure()
