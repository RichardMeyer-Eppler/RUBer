df_t1_and_t4_ex1 <- tibble::tribble(
  ~term,        ~students,              ~degree,              ~group, ~figure_type_id,
  "Spring '13",       120, "Bachelor 1-Subject",                  NA,              1L,
  "Spring '14",       105, "Bachelor 1-Subject",                  NA,              1L,
  "Spring '15",       124, "Bachelor 1-Subject",                  NA,              1L,
  "Spring '16",       114, "Bachelor 1-Subject",                  NA,              1L,
  "Spring '17",       122, "Bachelor 1-Subject",                  NA,              1L,
  "Spring '13",       121,   "Master 1-Subject",                  NA,              1L,
  "Spring '14",       129,   "Master 1-Subject",                  NA,              1L,
  "Spring '15",       122,   "Master 1-Subject",                  NA,              1L,
  "Spring '16",       168,   "Master 1-Subject",                  NA,              1L,
  "Spring '17",         7,   "Master 1-Subject",                  NA,              1L,
  "Spring '13",        20,                   NA, "Freshman students",              4L,
  "Spring '14",        30,                   NA, "Freshman students",              4L,
  "Spring '15",        41,                   NA, "Freshman students",              4L,
  "Spring '16",        27,                   NA, "Freshman students",              4L,
  "Spring '17",        35,                   NA, "Freshman students",              4L
)

rub_plot_type_1_and_4(
  df = df_t1_and_t4_ex1,
  x_var = term,
  y_var = students,
  fill_var = degree,
  group_var = group,
  base_family = "sans"
)
