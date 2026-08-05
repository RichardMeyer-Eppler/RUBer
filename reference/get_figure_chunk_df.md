# Get code chunks for plotting all figures

Get code chunks for plotting all figures

## Usage

``` r
get_figure_chunk_df(
  df,
  font_family = get_font_df()[["family"]],
  function_call = "RUBer::plot_figure",
  heading_starting_lvl = 1L
)
```

## Arguments

- df:

  Data frame with columns `figure_nr`

- font_family:

  Character, the font family to use for all plots, defaults to
  `get_font_df()[["family"]]`

- function_call:

  Character, the function call for each plot chunk, defaults to
  [`RUBer::plot_figure`](plot_figure.md)

- heading_starting_lvl:

  Integer, the level of the heading.

## Value

Data frame with columns `figure_nr`, `chunk_heading`,
`chunk_subheading`, `chunk_figure_df` and `chunk_plot_figure`

## Examples

``` r
get_figure_chunk_df(RUBer::df_example %>% dplyr::filter(report_nr == 1L))
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `chunk_plot_figure = tpl_plot_figure(...)`.
#> Caused by warning:
#> ! ✖ Font file "RubFlama-Regular.ttf" could not be found
#> ℹ Using fallback font "DejaVuSans" instead
#> This warning is displayed once per session.
#> # A tibble: 88 × 5
#>    figure_nr chunk_heading chunk_subheading chunk_figure_df chunk_plot_figure
#>        <int> <list>        <list>           <list>          <list>           
#>  1         1 <list [1]>    <chr [1]>        <chr [3]>       <list [1]>       
#>  2         2 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  3         3 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  4         4 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  5         5 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  6         6 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  7         7 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  8         8 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#>  9         9 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#> 10        10 <chr [1]>     <chr [1]>        <chr [3]>       <list [1]>       
#> # ℹ 78 more rows
```
