# Plot vertical stacked bar charts that are scaled to 100% (figure type 2)

Plot vertical stacked bar charts that are scaled to 100% (figure type 2)

## Usage

``` r
rub_plot_type_2(
  df,
  x_var,
  x_var_label = NULL,
  y_var,
  y_axis_label = "",
  fill_var,
  fill_label = NULL,
  fill_reverse = FALSE,
  facet_var = NULL,
  caption = "",
  caption_prefix = "Quelle:",
  filter_cutoff = 0.04,
  color = RUB_colors["blue"],
  palette_reverse = FALSE,
  base_family = get_font_df()[["family"]],
  base_size = 11,
  max_width_strip_label = 80
)
```

## Arguments

- df:

  Data frame

- x_var:

  Required variable name for the variable containing the discrete
  x-coordinates.

- x_var_label:

  Optional variable name for the character variable containing the names
  of the x variable, defaults to NULL.

- y_var:

  Required variable name for the variable containing the y-coordinates.
  Will be coerced to numeric with `as.numeric`.

- y_axis_label:

  Optional label for the y-axis, defaults to an empty string.

- fill_var:

  Variable name for the discrete variable which determines the groups to
  be stacked, e.g. degree.

- fill_label:

  Optional variable name for the character variable containing the names
  of the fill variable, defaults to NULL.

- fill_reverse:

  Boolean indicating whether the order of the fill variable should be
  reversed, defaults to FALSE.

- facet_var:

  Optional variable name for the discrete variable to facet by, defaults
  to NULL.

- caption:

  Optional character containing the data source for the figure (prefix
  'Quelle:' is automatically added).

- caption_prefix:

  Optional character containing the prefix for the caption, defaults to
  'Quelle:'.

- filter_cutoff:

  Optional cutoff value for the suppression of data labels. By default,
  all values below 0.04 of the total value of the stacked bar chart are
  suppressed.

- color:

  Color for font and borders, defaults to `RUB_colors["blue"]`, i.e.
  \#003560.

- palette_reverse:

  Optional boolean indicating whether the colors in the palette should
  be reversed, defaults to FALSE.

- base_family:

  base font family, defaults to RubFlama

- base_size:

  base font size, defaults to 11

- max_width_strip_label:

  Optional maximum width in characters for the facet label passed to
  ggplot2::label_wrap_gen.

## Value

A ggplot object

## See also

Other rub_plot_types:
[`rub_plot_type_1_and_4()`](rub_plot_type_1_and_4.md),
[`rub_plot_type_1()`](rub_plot_type_1.md),
[`rub_plot_type_3()`](rub_plot_type_3.md),
[`rub_plot_type_4()`](rub_plot_type_4.md)

## Examples

``` r
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
```
