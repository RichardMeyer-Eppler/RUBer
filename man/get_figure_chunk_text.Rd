% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_template.R
\name{get_figure_chunk_text}
\alias{get_figure_chunk_text}
\title{Turn data frame obtained by \code{get_figure_chunk_df} into character vector of
code chunks}
\usage{
get_figure_chunk_text(df)
}
\arguments{
\item{df}{Data frame obtained by \code{get_figure_chunk_df}}
}
\value{
Vector with chunk texts
}
\description{
Turn data frame obtained by \code{get_figure_chunk_df} into character vector of
code chunks
}
\examples{
get_figure_chunk_text(get_figure_chunk_df(RUBer::df_example \%>\% dplyr::filter(report_nr == 1L)))
}
