# Example data set illustrating the use of the RUBer parameterized reporting package, containing the data to generate all figures.

All data values in this data set are algorithmically generated. With
that exception, though, the data set closely resembles the confidential
data the package was developed for. It contains the data to generate 68
reports with varying numbers of figures each.

## Usage

``` r
df_example
```

## Format

A data frame with 164,794 rows and 24 variables:

- report_nr:

  Integer, report number used for filtering and joining

- figure_nr:

  Integer, unique identifier for each figure

- report_type_id:

  Character, one of `c("STG", "MED", "M_ED", "FGR", "SZMA")`. This ID is
  used for filtering and conditional logic.

- x:

  Character, x axis values. Will only be used for ordering if x_label is
  filled.

- x_label:

  Character, x axis values to be displayed

- y:

  Character, y axis value of the figure

- y_axis_label:

  Character, y axis label of the figure

- fill:

  Integer, values used for ordering of the fill

- fill_label:

  Character, values used for the fill labels

- facet:

  Character, values to use for facetting the figure

- group:

  Integer, values to use for the grouping

- group_label:

  Character, values to use as group labels

- source_caption:

  Character, the source of the figure's data, displayed at the
  bottom-right of each figure. Used as caption argument in the call to
  [`ggplot2::labs`](https://ggplot2.tidyverse.org/reference/labs.html).

- question_txt:

  Character, the question posed to the survey's respondents and
  displayed as figure title

- figure_type_id:

  Integer, ID determining the type of figure to plot. Currently one of
  four: 1 - stacked bar chart; 2 - vertical stacked bar chart scaled to
  100%; 3 - horizontal stacked bar chart scaled to 100%, 4 - line chart.

- figure_caption:

  Character, the figure caption displayed above each figure.

- heading:

  Character, the level 1 heading appearing above this figure, only
  displayed when the corresponding boolean is true.

- subheading:

  Character, the level 2 heading appearing above this figure, only
  displayed when the corresponding boolean is true.

- is_heading:

  Boolean, determines whether the level 1 heading is printed above the
  figure.

- is_subheading:

  Boolean, determines whether the level 2 heading is printed above the
  figure.

- report_author:

  Character, the report author, appearing on the title page, on the
  header of each page.

- report_title:

  Character, the report title, appearing on the title page and in the
  header of each page.

- file_name:

  Character, the file name for this report

- figure_height:

  Numeric, height of the figure in inches

## Details

Table: Data summary

|                                                  |            |
|--------------------------------------------------|------------|
|                                                  |            |
| Name                                             | df_example |
| Number of rows                                   | 164794     |
| Number of columns                                | 24         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 16         |
| logical                                          | 2          |
| numeric                                          | 6          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

**Variable type: character**

|                |           |               |     |     |       |          |            |
|----------------|-----------|---------------|-----|-----|-------|----------|------------|
| skim_variable  | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
| report_type_id | 0         | 1.00          | 3   | 4   | 0     | 5        | 0          |
| x              | 0         | 1.00          | 1   | 20  | 0     | 156768   | 0          |
| x_label        | 156748    | 0.05          | 9   | 16  | 0     | 20       | 0          |
| y              | 0         | 1.00          | 1   | 57  | 0     | 18591    | 0          |
| y_axis_label   | 161644    | 0.02          | 30  | 45  | 0     | 2        | 0          |
| fill_label     | 952       | 0.99          | 1   | 58  | 0     | 108      | 0          |
| facet          | 3150      | 0.98          | 9   | 168 | 0     | 1415     | 0          |
| group_label    | 163842    | 0.01          | 24  | 25  | 0     | 2        | 0          |
| source_caption | 0         | 1.00          | 46  | 76  | 0     | 8        | 0          |
| question_txt   | 9996      | 0.94          | 19  | 213 | 0     | 53       | 0          |
| figure_caption | 0         | 1.00          | 24  | 122 | 0     | 812      | 0          |
| heading        | 0         | 1.00          | 20  | 57  | 0     | 5        | 0          |
| subheading     | 32647     | 0.80          | 15  | 46  | 0     | 9        | 0          |
| report_author  | 0         | 1.00          | 10  | 178 | 0     | 68       | 0          |
| report_title   | 0         | 1.00          | 26  | 27  | 0     | 68       | 0          |
| file_name      | 0         | 1.00          | 29  | 30  | 0     | 68       | 0          |

**Variable type: logical**

|               |           |               |      |                         |
|---------------|-----------|---------------|------|-------------------------|
| skim_variable | n_missing | complete_rate | mean | count                   |
| is_heading    | 0         | 1             | 0.05 | FAL: 157293, TRU: 7501  |
| is_subheading | 0         | 1             | 0.08 | FAL: 151770, TRU: 13024 |

**Variable type: numeric**

|  |  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|
| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
| report_nr | 0 | 1.00 | 35.78 | 22.23 | 1.0 | 16.00 | 36.00 | 59.00 | 68.00 | ▇▅▇▃▇ |
| figure_nr | 0 | 1.00 | 87.63 | 105.12 | 1.0 | 29.00 | 56.00 | 93.00 | 550.00 | ▇▁▁▁▁ |
| fill | 952 | 0.99 | 3.02 | 2.17 | 0.0 | 2.00 | 3.00 | 4.00 | 22.00 | ▇▂▁▁▁ |
| group | 7094 | 0.96 | 81.98 | 15.49 | 8.0 | 81.00 | 82.00 | 88.00 | 99.00 | ▁▁▁▁▇ |
| figure_type_id | 0 | 1.00 | 2.95 | 0.29 | 1.0 | 3.00 | 3.00 | 3.00 | 4.00 | ▁▁▁▇▁ |
| figure_height | 0 | 1.00 | 6.59 | 2.12 | 1.3 | 4.92 | 7.28 | 8.54 | 9.45 | ▁▅▃▆▇ |
