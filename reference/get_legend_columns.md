# Gets appropriate number of legend columns based on the plot, font and active graphics device

Gets appropriate number of legend columns based on the plot, font and
active graphics device

## Usage

``` r
get_legend_columns(
  legend_text,
  y_axis_text,
  legend_key_width = plot_width/100,
  legend_key_spacing = plot_width/100,
  plot_width = 6.8,
  base_size = 11,
  base_family = get_font_df()[["family"]],
  systemfonts_suffix = "_systemfonts"
)
```

## Arguments

- legend_text:

  Vector with the legend text

- y_axis_text:

  Vector with the text labels of the y axis

- legend_key_width:

  Legend key width

- legend_key_spacing:

  Legend key spacing

- plot_width:

  Width of the plot in inches, defaults to 6.8

- base_size:

  base font size, defaults to 11

- base_family:

  base font family, defaults to RubFlama

- systemfonts_suffix:

  Suffix attached to the font family name in
  [`systemfonts::register_font`](https://systemfonts.r-lib.org/reference/register_font.html)

## Value

Numeric with the number of columns for the legend

## References

- [Unneeded warnings when creating plot using non-default fonts
  \#729](https://github.com/yihui/knitr/issues/729)

- [chunk_device in
  block.R](https://github.com/yihui/knitr/blob/master/R/block.R)

- [Access to chunk label \#73](https://github.com/yihui/knitr/issues/73)

## Examples

``` r
get_legend_columns(
  legend_text = c(
    "1 - eigener Verdienst",
    "2 - Mittel der Eltern/Verwandten",
    "3 - Förderung nach BAföG",
    "4 - Stipendium",
    "5 - Sonstiges"
  ),
  y_axis_text = c(
    "Bachelor 2-Fächer (n=251)",
    "FG Bachelor 2-Fächer (n=1.310)"
  )
)
#> Warning: devEMF: your system substituted font family 'DejaVu Sans' when you requested 'DejaVu Sans_systemfonts'
#> [1] 1
```
