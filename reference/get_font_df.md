# Get data frame of font information for a font file

Get data frame of font information for a font file

## Usage

``` r
get_font_df(font_file = "RubFlama-Regular.ttf")
```

## Arguments

- font_file:

  Character, name of the font file (case insensitive)

## Value

Data frame obtained by
[`systemfonts::system_fonts`](https://systemfonts.r-lib.org/reference/system_fonts.html)
with one row

## Examples

``` r
get_font_df("RUB Scala TZ.ttf")
#> Warning: ✖ Font file "RUB Scala TZ.ttf" could not be found
#> ℹ Using fallback font "DejaVuSans" instead
#> This warning is displayed once per session.
#> # A tibble: 1 × 10
#>   path           index name  family style weight width italic monospace variable
#>   <chr>          <int> <chr> <chr>  <chr> <ord>  <ord> <lgl>  <lgl>     <lgl>   
#> 1 /usr/share/fo…     0 Deja… DejaV… Book  normal norm… FALSE  FALSE     FALSE   
```
