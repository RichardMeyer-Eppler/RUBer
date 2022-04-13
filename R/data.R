#' Example data set illustrating the use of the RUBer parameterized
#' reporting package, containing the data to generate all figures.
#'
#' All data values in this data set are algorithmically generated. With that
#' exception, though, the data set closely resembles the confidential data the
#' package was developed for. It contains the data to generate
#' `r dplyr::n_distinct(df_example[["report_nr"]])` reports with varying numbers
#' of figures each.
#'
#' @format A data frame with `r format(nrow(df_example),big.mark=",")` rows and `r ncol(df_example)` variables:
#' \describe{
#'   \item{report_nr}{Integer, report number used for filtering and joining}
#'   \item{figure_count}{Integer, unique identifier for each figure}
#'   \item{report_type_id}{Character, one of `c("STG", "MED", "M_ED", "FGR")`.
#'           This ID is used for filtering and conditional logic.}
#'   \item{x}{Character, x axis values. Will only be used for ordering if
#'           x_label is filled.}
#'   \item{x_label}{Character, x axis values to be displayed}
#'   \item{y}{Character, y axis value of the figure}
#'   \item{y_label}{Character, y axis label of the figure}
#'   \item{fill}{Integer, values used for ordering of the fill}
#'   \item{fill_label}{Character, values used for the fill labels}
#'   \item{fill_reverse}{Boolean, if `TRUE` the fill ordering will be reversed
#'          when plotting.}
#'   \item{facet}{Character, values to use for facetting the figure}
#'   \item{group}{Integer, values to use for the grouping}
#'   \item{group_label}{Character, values to use as group labels}
#'   \item{source_caption}{Character, the source of the figure's data, displayed
#'           at the bottom-right of each figure. Used as caption argument in the
#'           call to `ggplot2::labs`.}
#'   \item{question_txt}{Character, the question posed to the survey's
#'           respondents and displayed as figure title}
#'   \item{figure_type_id}{Integer, ID determining the type of figure to plot.
#'           Currently one of four: 1 - stacked bar chart; 2 - vertical stacked
#'           bar chart scaled to 100%; 3 - horizontal stacked bar chart scaled
#'           to 100%, 4 - line chart.}
#'   \item{figure_caption}{Character, the figure caption displayed above each
#'           figure.}
#'   \item{heading}{Character, the level 1 heading appearing above this figure,
#'           only displayed when the corresponding boolean is true.}
#'   \item{subheading}{Character, the level 2 heading appearing above this
#'           figure, only displayed when the corresponding boolean is true.}
#'   \item{is_heading}{Boolean, determines whether the level 1 heading is
#'           printed above the figure.}
#'   \item{is_subheading}{Boolean, determines whether the level 2 heading is
#'           printed above the figure.}
#'   \item{report_author}{Character, the report author, appearing on the title
#'           page, on  the header of each page.}
#'   \item{report_title}{Character, the report title, appearing on the title
#'           page and in the header of each page.}
#'   \item{file_name}{Character, the file name for this report}
#'   \item{figure_height}{Numeric, height of the figure in inches}
#' }
#' @details
#' ```{r, results = "asis", echo = FALSE}
#' skimr::skim(df_example)
#' ```
"df_example"

#' Example data set illustrating the use of the RUBer parameterized
#' reporting package, containing the metadata for each report. The metadata
#' includes the author, the title, the file name, and the subfolder to be used
#' in the output path.
#'
#' @format A data frame with `r format(nrow(df_report),big.mark=",")` rows and `r ncol(df_report)` variables:
#' \describe{
#'   \item{report_nr}{Integer, report number used for filtering and joining}
#'   \item{report_type_id}{Character, one of `c("STG", "MED", "M_ED", "FGR")`.
#'           This ID is used for filtering and conditional logic.}
#'   \item{report_title}{Character, the report title, appearing on the title
#'           page and in the header of each page.}
#'   \item{report_author}{Character, the report author, appearing on the title
#'           page and in the header of each page.}
#'   \item{file_name}{Character, the file name for this report}
#'   \item{subfolder}{Character, the subfolder to include in the output path}
#' }
#' @details
#' ```{r, results = "asis", echo = FALSE}
#' skimr::skim(df_report)
#' ```
"df_report"
