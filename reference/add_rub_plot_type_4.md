# Helper Function

Helper Function

## Usage

``` r
add_rub_plot_type_4(
  df_t4,
  x_var,
  x_var_label = NULL,
  y_var,
  group_var,
  group_label = NULL,
  base_size = 11,
  base_family = get_font_df()[["family"]],
  color = RUB_colors["blue"],
  palette_reverse = FALSE,
  legend_columns = 5
)
```

## Arguments

- df_t4:

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

- group_var:

  Variable name for the discrete variable which determines the groups
  forming one line, e.g. degree_id.

- group_label:

  Optional variable name for the character variable containing the names
  of the group variable (e.g. degree_txt), defaults to NULL.

- base_size:

  base font size, defaults to 11

- base_family:

  base font family, defaults to RubFlama

- color:

  Color for font and borders, defaults to `RUB_colors["blue"]`, i.e.
  \#003560.

- palette_reverse:

  Optional boolean indicating whether the colors in the palette should
  be reversed, defaults to FALSE.

## Value

List of ggplot2 expressions

## Examples

``` r
if (FALSE) { # \dontrun{
add_rub_plot_type_4(df, x, y, group, group_label)
} # }
```
