# Add centered y-coordinates for filtered data labels of figure types 1 and 2

Calculates the y-coordinates for stacked bar charts, centered for each
group. The default function does not work if some labels are filtered.

## Usage

``` r
add_label_position(
  df,
  x_var,
  y_var,
  facet_var = NULL,
  fill_var,
  filter_cutoff = 0.05,
  fill_reverse = FALSE,
  is_percentage = FALSE
)
```

## Arguments

- df:

  Data frame

- x_var:

  Required variable name for the variable containing the discrete
  x-coordinates.

- y_var:

  Required variable name for the variable containing the y-coordinates.
  Will be coerced to numeric with `as.numeric`.

- facet_var:

  Optional variable name for the discrete variable to facet by, defaults
  to NULL.

- fill_var:

  Variable name for the discrete variable which determines the groups to
  be stacked, e.g. degree.

- filter_cutoff:

  Optional cutoff value for the suppression of data labels. By default,
  all values below 0.04 of the total value of the stacked bar chart are
  suppressed.

- fill_reverse:

  Boolean indicating whether the order of the fill variable should be
  reversed, defaults to FALSE.

- is_percentage:

  Optional boolean indicating whether the value label is expressed in
  absolute numbers or as a percentage, defaults to false

## Value

Data frame with additional column "label\_" + the name of the
y-coordinate variable

## Examples

``` r
if (FALSE) { # \dontrun{
add_label_position(df, x_var, y_var, fill)
} # }
```
