# Get system dependent fallback font if a given font is not available

This functions returns the system dependent font for the alias "sans".

## Usage

``` r
get_fallback_font_df(
  fonts = systemfonts::system_fonts(),
  fallback_alias = "sans"
)
```

## Arguments

- fonts:

  Data frame obtained by
  [`systemfonts::system_fonts`](https://systemfonts.r-lib.org/reference/system_fonts.html)

- fallback_alias:

  Character, one of `c("sans", "serif", "mono", "emoji")` defaults to
  "sans".

## Value

Data frame obtained by
[`systemfonts::system_fonts`](https://systemfonts.r-lib.org/reference/system_fonts.html)
with one row

## Details

The aliases are mapped the following way:

- "" and "sans" return Helvetica on Mac, Arial on Windows, and the
  default sans-serif font on Linux (DejaVu Sans on Ubuntu)

- "serif" return Times on Mac, Times New Roman on Windows, and the
  default serif font on Linux (DejaVu Serif on Ubuntu)

- "mono" return Courier on Mac, Courier New on Windows, and the default
  mono font on Linux (DejaVu Mono on Ubuntu)

- "emoji" return Apple Color Emoji on Mac, Segoe UI Emoji on Windows,
  and the default emoji font on Linux (Noto Color on Ubuntu)

See https://github.com/r-lib/systemfonts for details

## Examples

``` r
get_fallback_font_df()
#> # A tibble: 1 × 10
#>   path           index name  family style weight width italic monospace variable
#>   <chr>          <int> <chr> <chr>  <chr> <ord>  <ord> <lgl>  <lgl>     <lgl>   
#> 1 /usr/share/fo…     0 Deja… DejaV… Book  normal norm… FALSE  FALSE     FALSE   
```
