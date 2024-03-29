% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_example_as_chunk.R
\name{run_example_as_chunk}
\alias{run_example_as_chunk}
\title{Knits an R source file contained in the examples folder of a package}
\usage{
run_example_as_chunk(example_file, chunk_name, package = "RUBer")
}
\arguments{
\item{example_file}{Character, name of the R script file with the example}

\item{chunk_name}{Character, name of the code chunk when knitting}

\item{package}{Character, name of the package}
}
\value{
Character, text to be inserted in the Markdown file
}
\description{
Knits an R source file contained in the examples folder of a package
}
\examples{
RUBer:::run_example_as_chunk(
  example_file = "rub_plot_type_1.R",
  chunk_name = "rub-plot-type-1",
  package = "RUBer"
)
}
\keyword{internal}
