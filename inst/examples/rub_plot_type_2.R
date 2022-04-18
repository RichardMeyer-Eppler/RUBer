# Create test data for all three mandatory variables (x_var, y_var, fill_var)
df_t2_ex1 <- tibble::tribble(
  ~cohort_term,     ~status_percentage,               ~cohort_status,
  "2. cohort term",              0.951,                   "Studying",
  "2. cohort term",              0.003,            "Changed subject",
  "2. cohort term",                  0,                  "Graduated",
  "2. cohort term",              0.019, "Disenrolled without degree",
  "2. cohort term",              0.027,            "Dropped subject",
  "4. cohort term",               0.89,                   "Studying",
  "4. cohort term",              0.062,            "Changed subject",
  "4. cohort term",              0.002,                  "Graduated",
  "4. cohort term",               0.02, "Disenrolled without degree",
  "4. cohort term",              0.027,            "Dropped subject",
  "6. cohort term",               0.79,                   "Studying",
  "6. cohort term",               0.15,            "Changed subject",
  "6. cohort term",              0.007,                  "Graduated",
  "6. cohort term",              0.024, "Disenrolled without degree",
  "6. cohort term",              0.028,            "Dropped subject",
  "8. cohort term",              0.612,                   "Studying",
  "8. cohort term",              0.296,            "Changed subject",
  "8. cohort term",               0.01,                  "Graduated",
  "8. cohort term",              0.027, "Disenrolled without degree",
  "8. cohort term",              0.054,            "Dropped subject"
)

rub_plot_type_2(
  df = df_t2_ex1,
  x_var = cohort_term,
  y_var = status_percentage,
  fill_var = cohort_status,
  base_family = "sans"
)
