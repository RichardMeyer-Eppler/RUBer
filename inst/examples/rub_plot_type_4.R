# Create test data for all three mandatory variables (x_var, y_var,
# group_var)
df_t4_ex1 <- tibble::tribble(
  ~term,        ~students,              ~degree,
  "Spring '13",       110, "Bachelor 1-Subject",
  "Spring '14",       105, "Bachelor 1-Subject",
  "Spring '15",       124, "Bachelor 1-Subject",
  "Spring '16",       114, "Bachelor 1-Subject",
  "Spring '17",       140, "Bachelor 1-Subject",
  "Spring '13",       121,   "Master 1-Subject",
  "Spring '14",       129,   "Master 1-Subject",
  "Spring '15",       135,   "Master 1-Subject",
  "Spring '16",       168,   "Master 1-Subject",
  "Spring '17",         7,   "Master 1-Subject"
)

rub_plot_type_4(
  df = df_t4_ex1,
  x_var = term,
  y_var = students,
  group_var = degree,
  y_axis_label = stringr::str_wrap(
    string = "Students (1st degree program, 1st and 2nd field of study)",
    width = 35
  ),
  caption = "Vignette example data",
  caption_prefix = "Source:",
  base_family = "sans"
)
