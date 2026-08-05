# Prepares var and var_label columns for plotting by ordering them and turning them into factors

Prepares var and var_label columns for plotting by ordering them and
turning them into factors

## Usage

``` r
set_factor_var(df, var, var_label = NULL, reverse = FALSE)
```

## Arguments

- df:

  Data frame

- var:

  Required variable name for the discrete variable to be turned into a
  factor. This variable is sorted alphabetically to determine the order.

- var_label:

  Optional variable name for the discrete variable labels to be used
  instead of var. If var is a sort key, for instance, var_label can be
  used for the actual labels.

- reverse:

  Whether the order of the factor should be reverted, defaults to FALSE.

## Value

Data frame with factor column.

## Examples

``` r
if (FALSE) { # \dontrun{
set_factor_var(df, var, TRUE)
} # }
```
