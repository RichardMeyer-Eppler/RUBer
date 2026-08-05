# Filter data frame based on `report_nr`

Filter data frame based on `report_nr`

## Usage

``` r
filter_report(df, report_nr)
```

## Arguments

- df:

  Data frame

- report_nr:

  Required integer indicating the report_nr

## Value

Filtered data frame with data for the report nr

## Examples

``` r
filter_report(df_example, report_nr = 12)
#> # A tibble: 1,656 × 24
#>    report_nr figure_nr report_type_id x     x_label    y     y_axis_label   fill
#>        <int>     <int> <chr>          <chr> <chr>      <chr> <chr>         <dbl>
#>  1        12         1 STG            20152 WiSe 15/16 110   Studienfälle…     1
#>  2        12         1 STG            20152 WiSe 15/16 652   Studienfälle…    20
#>  3        12         1 STG            20162 WiSe 16/17 112   Studienfälle…     1
#>  4        12         1 STG            20162 WiSe 16/17 560   Studienfälle…    20
#>  5        12         1 STG            20172 WiSe 17/18 128   Studienfälle…     1
#>  6        12         1 STG            20172 WiSe 17/18 568   Studienfälle…    20
#>  7        12         1 STG            20182 WiSe 18/19 108   Studienfälle…     1
#>  8        12         1 STG            20182 WiSe 18/19 495   Studienfälle…    20
#>  9        12         1 STG            20192 WiSe 19/20 115   Studienfälle…     1
#> 10        12         1 STG            20192 WiSe 19/20 579   Studienfälle…    20
#> # ℹ 1,646 more rows
#> # ℹ 16 more variables: fill_label <chr>, facet <chr>, group <dbl>,
#> #   group_label <chr>, source_caption <chr>, question_txt <chr>,
#> #   figure_type_id <int>, figure_caption <glue>, heading <chr>,
#> #   subheading <chr>, is_heading <lgl>, is_subheading <lgl>,
#> #   report_author <chr>, report_title <glue>, file_name <glue>,
#> #   figure_height <dbl>
```
