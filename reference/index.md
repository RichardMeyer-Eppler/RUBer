# Package index

## Plotting functions

Plotting functions for each `figure_type_id`.

- [`plot_figure()`](plot_figure.md) : Plot RUB figure
- [`rub_plot_type_1()`](rub_plot_type_1.md) : Plot vertical stacked bar
  chart (figure type 1)
- [`rub_plot_type_1_and_4()`](rub_plot_type_1_and_4.md) : Plot grouped
  line chart on top of vertical stacked bar chart (combination chart of
  figure types 1 and 4)
- [`rub_plot_type_2()`](rub_plot_type_2.md) : Plot vertical stacked bar
  charts that are scaled to 100% (figure type 2)
- [`rub_plot_type_3()`](rub_plot_type_3.md) : Plot horizontal stacked
  bar charts that are scaled to 100% (figure type 3)
- [`rub_plot_type_4()`](rub_plot_type_4.md) : Plot grouped line chart
  (figure type 4)
- [`get_legend_columns()`](get_legend_columns.md) : Gets appropriate
  number of legend columns based on the plot, font and active graphics
  device

## Scaling

Scale functions for use with `ggplot2`.

- [`scale_color_rub()`](scale_color_rub.md) : Color scale constructor
  for RUB colors
- [`scale_fill_rub()`](scale_fill_rub.md) : Fill scale constructor for
  RUB colors

## Colors and palettes

Functions for retrieving the palettes and colors of the RUB corporate
design, see the vignette on `Using RUB colors` for details.

- [`get_RUB_colors()`](get_RUB_colors.md) : Function to extract RUB
  palette colors as hex codes
- [`get_RUB_palettes()`](get_RUB_palettes.md) : Return function to
  interpolate a RUB color palette
- [`RUB_colors`](RUB_colors.md) : RUB colors
- [`RUB_palettes`](RUB_palettes.md) : RUB color palette

## Reporting functions

Functions for creating the parametric reports and associated helper
functions, such as filter and file path functions.

- [`get_file_path()`](get_file_path.md) : Get file path for automatic
  report generation

- [`get_report_nr_by_id()`](get_report_nr_by_id.md) :

  Get all unique values of `report_nr` for a `report_type_id`

- [`filter_report()`](filter_report.md) :

  Filter data frame based on `report_nr`

- [`render_report()`](render_report.md)
  [`render_report_safely()`](render_report.md) : Render a single
  parametric report as Word file

## Templating Functions

Functions for creating the code chunks required for the figures and the
dynamic placement of headings.

- [`get_figure_chunk_df()`](get_figure_chunk_df.md) : Get code chunks
  for plotting all figures

- [`get_figure_chunk_text()`](get_figure_chunk_text.md) :

  Turn data frame obtained by `get_figure_chunk_df` into character
  vector of code chunks

- [`tpl_get_figure_df()`](tpl_get_figure_df.md) : Returns a character
  vector for the code chunk to retrieve the figure data frame

- [`tpl_heading()`](tpl_heading.md) : Returns a character vector for the
  code chunk to insert a heading

- [`tpl_subheading()`](tpl_subheading.md) : Returns a character vector
  for the code chunk to insert a subheading

- [`tpl_plot_figure()`](tpl_plot_figure.md) : Returns a character vector
  for the code chunk to plot a figure

## Theming functions

Theming functions for ggplot2 objects, flextable objects and flextable
cells.

- [`rub_style_flextable()`](rub_style_flextable.md) : Applies RUB theme
  to flextable object
- [`theme_rub()`](theme_rub.md) : Add RUB theme to ggplot object
- [`rub_format_mixed()`](rub_format_mixed.md) : Format flextable mixed
  type columns (integer, percentages, NAs)
- [`rub_format_percent()`](rub_format_percent.md) : Format flextable
  columns with percentages (percentages, NAs)

## Table functions

These functions take data frames as input and return formatted flextable
objects.

- [`rub_table_ab()`](rub_table_ab.md) : Get formatted flextable of
  response rates for the Absolvent:innenbefragung
- [`rub_table_eb()`](rub_table_eb.md) : Get formatted flextable of
  response rates for the Eingangsbefragung
- [`rub_table_excluded_programs()`](rub_table_excluded_programs.md) :
  Get formatted flextable of excluded programs
- [`rub_table_included_programs()`](rub_table_included_programs.md) :
  Get formatted flextable of included programs
- [`rub_table_item()`](rub_table_item.md) : Get formatted flextable of
  items with largest deviation from comparison group
- [`rub_table_metrics()`](rub_table_metrics.md) : Get formatted
  flextable of metrics
- [`rub_table_programs()`](rub_table_programs.md) : Get formatted
  flextable of funded projects
- [`rub_table_stg()`](rub_table_stg.md) : Get formatted flextable of
  student cases
- [`rub_table_vb()`](rub_table_vb.md) : Get formatted flextable of
  response rates for the Verlaufsbefragung

## Font functions

These functions load the RUB corporate design fonts RUB Flama and RUB
Scala TZ. The package `showtext` is used to make the fonts available to
`ggplot2`.

- [`get_fallback_font_df()`](get_fallback_font_df.md) : Get system
  dependent fallback font if a given font is not available

- [`get_font_df()`](get_font_df.md) : Get data frame of font information
  for a font file

- [`register_font_df()`](register_font_df.md) :

  Register font using
  [`sysfonts::font_add`](https://rdrr.io/pkg/sysfonts/man/font_add.html)
  and
  [`systemfonts::register_font`](https://systemfonts.r-lib.org/reference/register_font.html)

- [`register_font_flama()`](register_font_flama.md) :

  Registers RUB Flama font to be used with the `showtext` and
  `systemfonts` packages

- [`register_font_scala()`](register_font_scala.md) :

  Registers RUB Scala TZ font to be used with `showtext` and
  `systemfonts` packages

## Data

Two data sets: `df_example` containing algorithmically generated data to
produce varying numbers of figures for each report and to place headings
and subheadings dynamically. `df_report` containing the meta data for
each report, such as the file name or the report author.

- [`df_example`](df_example.md) : Example data set illustrating the use
  of the RUBer parameterized reporting package, containing the data to
  generate all figures.
- [`df_report`](df_report.md) : Example data set illustrating the use of
  the RUBer parameterized reporting package, containing the metadata for
  each report.
