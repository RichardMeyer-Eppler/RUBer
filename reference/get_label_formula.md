# Get formula for calculating position of value labels

Get formula for calculating position of value labels

## Usage

``` r
get_label_formula(label_var, is_percentage = FALSE)
```

## Arguments

- label_var:

  The name of the variable requiring value labels

- is_percentage:

  Optional boolean indicating whether the value label is expressed in
  absolute numbers or as a percentage, defaults to false

## Value

A defused expression for calculating the position of the y-label

## Examples

``` r
if (FALSE) { # \dontrun{
get_label_formula(y_var = cyl, label_reverse = TRUE, is_percentage = FALSE)
} # }
```
