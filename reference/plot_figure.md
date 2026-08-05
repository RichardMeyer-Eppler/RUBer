# Plot RUB figure

Plot RUB figure

## Usage

``` r
plot_figure(df, font_family = get_font_df()[["family"]])
```

## Arguments

- df:

  Data Frame

- font_family:

  Character, the font family to use for all plots, defaults to
  `get_font_df()[["family"]]`

## Value

ggplot object

## Examples

``` r

RUBer::df_example %>%
  dplyr::filter(
    .data[["report_nr"]] == 6L,
    .data[["figure_nr"]] == 1L
  ) %>%
  RUBer::plot_figure(
    font_family = "sans"
  )
```
