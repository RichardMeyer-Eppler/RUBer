# Get all unique values of `report_nr` for a `report_type_id`

Get all unique values of `report_nr` for a `report_type_id`

## Usage

``` r
get_report_nr_by_id(df, report_type_id)
```

## Arguments

- df:

  Data frame with columns `report_nr` and `report_type_id`

- report_type_id:

  Any of `c("STG", "M_ED", "MED", "FGR", "SZMA")`

## Value

Integer vector

## Examples

``` r
get_report_nr_by_id(df = df_example, report_type_id = "FGR")
#> [1] 67
```
