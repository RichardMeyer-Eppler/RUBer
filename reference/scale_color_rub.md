# Color scale constructor for RUB colors

Color scale constructor for RUB colors

## Usage

``` r
scale_color_rub(palette = "discrete", discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in `RUB_palettes`

- discrete:

  Boolean indicating whether color aesthetic is discrete or not,
  defaults to TRUE.

- reverse:

  Optional boolean indicating whether the palette should be reversed,
  defaults to FALSE.

- ...:

  Additional arguments passed to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or `scale_gradient`, used when discrete is TRUE or FALSE, respectively

## Examples

``` r
ggplot2::ggplot(
  data = mtcars,
  ggplot2::aes(
    x = mpg,
    y = disp,
    color = as.factor(carb)
    )
  ) +
  ggplot2::geom_point() +
  scale_color_rub(
    palette = "discrete"
  )
```
