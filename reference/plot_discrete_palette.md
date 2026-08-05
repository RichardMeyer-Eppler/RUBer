# Get discrete palette for the plot

At the moment, RUB_palettes has discrete palettes for up to eight unique
colors. Above that number, colors are interpolated.

## Usage

``` r
plot_discrete_palette(colors_n)
```

## Arguments

- colors_n:

  Integer for the number of requestes colors in the discrete palette.

## Value

Name of the appropriate discrete palette from RUB_palettes

## Examples

``` r
RUBer:::plot_discrete_palette(12)
#> Warning: Number of requested colors for discrete palette exceeds eight.
#>       No predefined palette for more than eight discrete colors exists
#>       in RUB_palettes. Additional colors will be interpolated.
#> [1] "discrete_8"
```
