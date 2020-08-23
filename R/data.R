#' Fake data set illustrating the use of the RUBer parameterized
#' reporting package.
#'
#' All data values in this data set are algorithmically generated. With that
#' exception, though, the data set closely resembles the confidential data the
#' package was developed for. It contains the data to generate 64 reports with
#' varying numbers of figures each.
#'
#' @format A data frame with 249,492 rows and 24 variables:
#' \describe{
#'   \item{report_nr}{price, in US dollars}
#'   \item{figure_count}{weight of the diamond, in carats}
#'   \item{x}{weight of the diamond, in carats}
#'   \item{y}{weight of the diamond, in carats}
#'   \item{y_label}{weight of the diamond, in carats}
#'   \item{fill}{weight of the diamond, in carats}
#'   \item{fill_label}{weight of the diamond, in carats}
#'   \item{fill_reverse}{weight of the diamond, in carats}
#'   \item{facet}{weight of the diamond, in carats}
#'   \item{group}{weight of the diamond, in carats}
#'   \item{group_label}{weight of the diamond, in carats}
#'   \item{source_caption}{The source of the figure's data, displayed at the
#'           bottom-right of each figure. Used as caption argument in the call
#'           to \code{\link[ggplot2]{labs}}.}
#'   \item{question_txt}{The question posed to the survey's respondents.
#'           Displayed below the figure, below the figure caption.}
#'   \item{figure_type_id}{ID determining the type of figure to plot. Currently
#'           one of four: 1 - stacked bar chart; 2 - vertical stacked bar chart
#'           scaled to 100%; 3 - horizontal stacked bar chart scaled to 100%,
#'           4 - line chart.}
#'   \item{figure_caption}{The figure caption displayed below each figure.}
#'   \item{heading}{The level 1 heading for this figure, only displayed when
#'           the corresponding boolean is true.}
#'   \item{subheading}{The level 2 heading for this figure, only displayed when
#'           the corresponding boolean is true.}
#'   \item{is_heading}{Determines whether the level 1 heading is printed above
#'           the figure.}
#'   \item{is_subheading}{Determines whether the level 2 heading is printed above
#'           the figure.}
#'   \item{report_author}{The report author, appearing on the title page, on the
#'           header of each page.}
#'   \item{report_title}{The report title, appearing on the title page, on the
#'           header of each page.}
#'   \item{file_name}{The file name for this report nr.}
#'   \item{figure_filter_flag}{weight of the diamond, in carats}
#'   \item{figure_height}{The height of the figure.}
#' }
"df_fake"

#' Table series retrieved from...
#'
#' See vignette for details...
#'
"db_nrw_213"

