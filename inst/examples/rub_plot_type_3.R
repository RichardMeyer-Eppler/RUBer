# Create test data for all three mandatory variables (x_var, y_var,
# fill_var)
df_t3_ex1 <- tibble::tribble(
  ~survey_group,                                             ~item_value, ~item_value_percentage,
  "Bachelor 1-Subject (n=400)",    "Exceeded prescribed period of study",                    0.3,
  "Bachelor 1-Subject (n=400)",      "Within prescribed period of study",                    0.7,
  "SG Bachelor 1-Subject (n=669)", "Exceeded prescribed period of study",                   0.11,
  "SG Bachelor 1-Subject (n=669)",   "Within prescribed period of study",                   0.89
)

rub_plot_type_3(
   df = df_t3_ex1,
   x_var = item_value_percentage,
   y_var = survey_group,
   fill_var = item_value,
   base_family = "sans"
)

datapasta::tribble_paste(
  df_t3_ex1
)
