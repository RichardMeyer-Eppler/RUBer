# Plot grouped line chart (figure type 4)

Plot grouped line chart (figure type 4)

## Usage

``` r
rub_plot_type_4(
  df,
  x_var,
  x_var_label = NULL,
  x_axis_label = "",
  y_var,
  y_axis_label = "",
  group_var,
  group_label = NULL,
  caption = "",
  caption_prefix = "Quelle:",
  filter_cutoff = 5,
  facet_var = NULL,
  color = RUB_colors["blue"],
  palette_reverse = FALSE,
  base_family = get_font_df()[["family"]],
  base_size = 11
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

- x_axis_label:

  Optional label for the x-axis, defaults to an empty string.

- y_var:

  Required variable name for the variable containing the y-coordinates.
  Will be coerced to numeric with `as.numeric`.

- y_axis_label:

  Optional label for the y-axis, defaults to an empty string.

- group_var:

  Variable name for the discrete variable which determines the groups
  forming one line, e.g. degree_id.

- group_label:

  Optional variable name for the character variable containing the names
  of the group variable (e.g. degree_txt), defaults to NULL.

- caption:

  Optional character containing the data source for the figure (prefix
  'Quelle:' is automatically added).

- caption_prefix:

  Optional character containing the prefix for the caption, defaults to
  'Quelle:'.

- filter_cutoff:

  Optional integer marking the cutoff below which all value labels are
  suppressed, defaults to 5.

- facet_var:

  Optional variable name for the discrete variable to facet by, defaults
  to NULL.

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

## Value

A ggplot2 object

## See also

Other rub_plot_types:
[`rub_plot_type_1_and_4()`](rub_plot_type_1_and_4.md),
[`rub_plot_type_1()`](rub_plot_type_1.md),
[`rub_plot_type_2()`](rub_plot_type_2.md),
[`rub_plot_type_3()`](rub_plot_type_3.md)

## Examples

``` r
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
```
