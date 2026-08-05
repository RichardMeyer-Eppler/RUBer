# Example data set illustrating the use of the RUBer parameterized reporting package, containing the metadata for each report.

The metadata includes the author, the title, the file name, and the
subfolder to be used in the output path.

## Usage

``` r
df_report
```

## Format

A data frame with 68 rows and 6 variables:

- report_nr:

  Integer, report number used for filtering and joining

- report_type_id:

  Character, one of `c("STG", "MED", "M_ED", "FGR", "SZMA")`. This ID is
  used for filtering and conditional logic.

- report_title:

  Character, the report title, appearing on the title page and in the
  header of each page.

- report_author:

  Character, the report author, appearing on the title page and in the
  header of each page.

- file_name:

  Character, the file name for this report

- subfolder:

  Character, the subfolder to include in the output path

## Details

Table: Data summary

|                                                  |           |
|--------------------------------------------------|-----------|
|                                                  |           |
| Name                                             | df_report |
| Number of rows                                   | 68        |
| Number of columns                                | 6         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 5         |
| numeric                                          | 1         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

**Variable type: character**

|                |           |               |     |     |       |          |            |
|----------------|-----------|---------------|-----|-----|-------|----------|------------|
| skim_variable  | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
| report_type_id | 0         | 1.00          | 3   | 4   | 0     | 5        | 0          |
| report_title   | 0         | 1.00          | 26  | 27  | 0     | 68       | 0          |
| report_author  | 0         | 1.00          | 10  | 178 | 0     | 68       | 0          |
| file_name      | 0         | 1.00          | 29  | 30  | 0     | 68       | 0          |
| subfolder      | 3         | 0.96          | 5   | 39  | 0     | 7        | 0          |

**Variable type: numeric**

|               |           |               |      |       |     |       |      |       |      |       |
|---------------|-----------|---------------|------|-------|-----|-------|------|-------|------|-------|
| skim_variable | n_missing | complete_rate | mean | sd    | p0  | p25   | p50  | p75   | p100 | hist  |
| report_nr     | 0         | 1             | 34.5 | 19.77 | 1   | 17.75 | 34.5 | 51.25 | 68   | ▇▇▇▇▇ |
