# Format flextable columns with percentages (percentages, NAs)

Format flextable columns with percentages (percentages, NAs)

## Usage

``` r
rub_format_percent(x)
```

## Arguments

- x:

  Vector

## Value

Formatted vector

## Examples

``` r
x <- c("0.29", NA)
rub_format_percent(x)
#> [1] "29%" ""   
```
