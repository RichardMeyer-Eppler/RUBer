% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rub_tables.R
\name{rub_table_stg}
\alias{rub_table_stg}
\title{Get formatted flextable of student cases}
\usage{
rub_table_stg(df, label)
}
\arguments{
\item{df}{Data frame with columns \code{studiengang}, \code{studienfachzaehler}, \code{faelle}}

\item{label}{Label for the first column}
}
\value{
Formatted Flextable
}
\description{
Get formatted flextable of student cases
}
\section{Illustrations}{


\if{html}{\figure{rub_table_stg.png}{options: width=100\%}}
}

\examples{
rub_table_stg(
  df = tibble::tribble(
    ~studiengang, ~studienfachzaehler, ~faelle,
    "Studiengang A", "1. Fach", 1,
    "Studiengang B", "2. Fach", 1
  ),
  label = "Beispieltabelle"
)
}
