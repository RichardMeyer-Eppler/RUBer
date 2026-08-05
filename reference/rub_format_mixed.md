# Format flextable mixed type columns (integer, percentages, NAs)

Format flextable mixed type columns (integer, percentages, NAs)

## Usage

``` r
rub_format_mixed(x)
```

## Arguments

- x:

  Vector

## Value

Formatted vector

## Examples

``` r
x <- c("2500", "0.29", NA)
rub_format_mixed(x)
#> [1] "2.500" "29.0%" ""     
```
