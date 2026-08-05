# Return function to interpolate a RUB color palette

Return function to interpolate a RUB color palette

## Usage

``` r
get_RUB_palettes(palette = "discrete", reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in `RUB_palettes`

- reverse:

  Optional boolean indicating whether the palette should be reversed,
  defaults to FALSE.

- ...:

  Additional arguments to pass to
  [`colorRamp`](https://rdrr.io/r/grDevices/colorRamp.html)

## Value

Color palette

## Examples

``` r
get_RUB_palettes(palette = "continuous")
#> function (n) 
#> {
#>     x <- ramp(seq.int(0, 1, length.out = n))
#>     if (ncol(x) == 4L) 
#>         rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
#>     else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
#> }
#> <bytecode: 0x5639244eb830>
#> <environment: 0x5639244e8b80>
```
