# Function to extract RUB palette colors as hex codes

Function to extract RUB palette colors as hex codes

## Usage

``` r
get_RUB_colors(...)
```

## Arguments

- ...:

  Character names of RUB palette colors

## Value

Vector with specified color hex codes

## Examples

``` r
get_RUB_colors("green", "blue")
#>     green      blue 
#> "#8DAE10" "#003560" 
get_RUB_colors(1, 3, 1)
#>     green  green_60     green 
#> "#8DAE10" "#BBCE70" "#8DAE10" 
```
