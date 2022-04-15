# Create test values for all three mandatory variables (x_var, y_var, fill_var).
df_t1_ex1 <- tibble::tribble(
  ~term, ~students, ~degree,
  "Spring '13", 120, "Bachelor 1-Subject",
  "Spring '14", 105, "Bachelor 1-Subject",
  "Spring '15", 124, "Bachelor 1-Subject",
  "Spring '16", 114, "Bachelor 1-Subject",
  "Spring '17", 122, "Bachelor 1-Subject",
  "Spring '13", 121, "Master 1-Subject",
  "Spring '14", 129, "Master 1-Subject",
  "Spring '15", 122, "Master 1-Subject",
  "Spring '16", 168, "Master 1-Subject",
  "Spring '17", 7, "Master 1-Subject",
)

# x_var is mapped to term, y_var to students, and the fill_var to degree.
# base_size increases the text sizes from the default, 11, to 14. The font
# family is changed from "RubFlama" to "sans" (available on all systems).
rub_plot_type_1(
  df = df_t1_ex1,
  x_var = term,
  y_var = students,
  fill_var = degree,
  base_size = 14,
  base_family = "sans"
)
