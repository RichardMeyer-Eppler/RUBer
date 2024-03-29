% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rub_plot.R
\name{rub_plot_type_3}
\alias{rub_plot_type_3}
\title{Plot horizontal stacked bar charts that are scaled to 100\% (figure type 3)}
\usage{
rub_plot_type_3(
  df,
  x_var,
  y_var,
  x_axis_label = NA_character_,
  fill_var,
  fill_label = NULL,
  fill_reverse = FALSE,
  legend_reverse = FALSE,
  facet_var = NULL,
  title = NA_character_,
  caption = "",
  caption_prefix = "Quelle:",
  filter_cutoff = 0.05,
  color = RUB_colors["blue"],
  palette_reverse = FALSE,
  base_family = get_font_df()[["family"]],
  base_size = 11,
  max_width_axis_text_y = 30,
  max_width_strip_label = 80,
  plot_width = 6.8
)
}
\arguments{
\item{df}{Data frame}

\item{x_var}{Required variable name for the variable containing the
x-coordinates. Will be coerced to numeric with \code{as.numeric}.}

\item{y_var}{Required variable name for the variable containing the
y-coordinates. Will be coerced to numeric with \code{as.numeric}.}

\item{x_axis_label}{Optional label for the x-axis, defaults to an empty
string.}

\item{fill_var}{Variable name for the discrete variable which determines the
groups to be stacked, e.g. degree.}

\item{fill_label}{Optional variable name for the character variable
containing the names of the fill variable, defaults to NULL.}

\item{fill_reverse}{Boolean indicating whether the order of the fill variable
should be reversed, defaults to FALSE.}

\item{legend_reverse}{Optional boolean indicating whether the legend should
be reverted, defaults to FALSE.}

\item{facet_var}{Optional variable name for the discrete variable to facet
by, defaults to NULL.}

\item{title}{Optional plot title}

\item{caption}{Optional character containing the data source for the figure
(prefix 'Quelle:' is automatically added).}

\item{caption_prefix}{Optional character containing the prefix for the
caption, defaults to 'Quelle:'.}

\item{filter_cutoff}{Optional cutoff value for the suppression of data
labels. By default, all values below 0.04 of the total value of the
stacked bar chart are suppressed.}

\item{color}{Color for font and borders, defaults to \code{RUB_colors["blue"]},
i.e. #003560.}

\item{palette_reverse}{Optional boolean indicating whether the colors in the
palette should be reversed, defaults to FALSE.}

\item{base_family}{base font family, defaults to RubFlama}

\item{base_size}{base font size, defaults to 11}

\item{max_width_axis_text_y}{Optional maximum width in characters for the
text of the y axis.}

\item{max_width_strip_label}{Optional maximum width in characters for the facet
label passed to ggplot2::label_wrap_gen.}

\item{plot_width}{Width of the plot in inches, defaults to 6.8}
}
\value{
A ggplot object
}
\description{
Plot horizontal stacked bar charts that are scaled to 100\% (figure type 3)
}
\examples{
# Create test data for all three mandatory variables (x_var, y_var,
# fill_var)
df_t3_ex1 <- tibble::tribble(
  ~survey_group,                                             ~item_value, ~item_value_percentage,
  "Bachelor 1-Subject (n=400)",    "Exceeded prescribed period of study",                    0.3,
  "Bachelor 1-Subject (n=400)",      "Within prescribed period of study",                    0.7,
  "SG Bachelor 1-Subject (n=669)", "Exceeded prescribed period of study",                   0.11,
  "SG Bachelor 1-Subject (n=669)",   "Within prescribed period of study",                   0.89
)

rub_plot_type_3(
   df = df_t3_ex1,
   x_var = item_value_percentage,
   y_var = survey_group,
   fill_var = item_value,
   base_family = "sans"
)
}
\seealso{
Other rub_plot_types: 
\code{\link{rub_plot_type_1_and_4}()},
\code{\link{rub_plot_type_1}()},
\code{\link{rub_plot_type_2}()},
\code{\link{rub_plot_type_4}()}
}
\concept{rub_plot_types}
