# Fill scale constructor for RUB colors

Fill scale constructor for RUB colors

## Usage

``` r
scale_fill_rub(palette = "discrete", discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in RUB_palettes

- discrete:

  Boolean indicating whether color aesthetic is discrete or not

- reverse:

  Boolean indicating whether the palette should be reversed

- ...:

  Additional arguments passed to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or `scale_gradient`, used when discrete is TRUE or FALSE, respectively

## Examples

``` r
ggplot2::ggplot(
  data = mtcars,
  ggplot2::aes(
    x = as.factor(gear),
    fill = as.factor(vs)
    )
  ) +
  ggplot2::geom_bar() +
  scale_fill_rub(
    palette = "discrete_2"
  )
```
