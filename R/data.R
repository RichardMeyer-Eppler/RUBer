#' Example data set illustrating the use of the RUBer parameterized
#' reporting package.
#'
#' All data values in this data set are algorithmically generated. With that
#' exception, though, the data set closely resembles the confidential data the
#' package was developed for. It contains the data to generate `r dplyr::n_distinct(df_example[["report_nr"]])`
#' reports with varying numbers of figures each.
#'
#' @format A data frame with `r format(nrow(df_example),big.mark=",")` rows and `r ncol(df_example)` variables:
#' \describe{
#'   \item{report_nr}{Integer, report number used for filtering and joining}
#'   \item{figure_count}{Integer, unique identifier for each figure}
#'   \item{report_type_id}{Character, one of `c("STG", "MED", "M_ED", "FGR")`.
#'           The ID is used for filtering and conditional logic.}
#'   \item{x}{Character, x axis value of the figure}
#'   \item{y}{Character, y axis value of the figure}
#'   \item{y_label}{Character, y axis label of the figure}
#'   \item{fill}{Double, values used for ordering of the fill}
#'   \item{fill_label}{Character, values used for the fill labels}
#'   \item{fill_reverse}{Boolean, if `TRUE` the fill ordering will be reversed
#'          when plotting.}
#'   \item{facet}{Character, values to facet the figure by}
#'   \item{group}{Double, values to group the figure by}
#'   \item{group_label}{Character, values to use as group labels}
#'   \item{source_caption}{Character, the source of the figure's data, displayed
#'           at the bottom-right of each figure. Used as caption argument in the
#'           call to \code{\link[ggplot2]{labs}}.}
#'   \item{question_txt}{Character, the question posed to the survey's
#'           respondents and displayed as figure title}
#'   \item{figure_type_id}{Integer, ID determining the type of figure to plot.
#'           Currently one of four: 1 - stacked bar chart; 2 - vertical stacked
#'           bar chart scaled to 100%; 3 - horizontal stacked bar chart scaled
#'           to 100%, 4 - line chart.}
#'   \item{figure_caption}{Character, the figure caption displayed above each
#'           figure.}
#'   \item{heading}{Character, the level 1 heading for this figure, only
#'           displayed when the corresponding boolean is true.}
#'   \item{subheading}{Character, the level 2 heading for this figure, only
#'           displayed when the corresponding boolean is true.}
#'   \item{is_heading}{Boolean, determines whether the level 1 heading is
#'           printed above the figure.}
#'   \item{is_subheading}{Boolean, determines whether the level 2 heading is
#'           printed above the figure.}
#'   \item{report_author}{Character, the report author, appearing on the title
#'           page, on  the header of each page.}
#'   \item{report_title}{Character, the report title, appearing on the title
#'           page, on the header of each page.}
#'   \item{file_name}{Character, the file name for this report}
#'   \item{figure_filter_flag}{Integer, if one the figure will be suppressed in
#'           the report output}
#'   \item{aggregation_id_1}{XXX}
#'   \item{aggregation_sort_1}{XXX}
#'   \item{abbildung_map_sort}{XXX}
#'   \item{wert_sort}{XXX}
#'   \item{figure_height}{Numeric, height of the figure in inches}
#' }
"df_example"

