#' Knits an R source file contained in the examples folder of a package
#'
#' @param package Character, name of the package
#' @param example_file Character, name of the R script file with the example
#' @param chunk_name Character, name of the code chunk when knitting
#'
#' @return Character, text to be inserted in the Markdown file
#' @keywords internal
#'
#' @example inst/examples/run_example_as_chunk.R
run_example_as_chunk <- function(
  example_file,
  chunk_name,
  package = "RUBer"
) {
  src <- c(
    glue::glue(
      "```{r <<chunk_name>>}\n",
      .open = "<<",
      .close = ">>",
      .trim = FALSE
    ),
    readLines(
      system.file(
        "examples",
        example_file,
        package = "RUBer"
      )
    ),
    "```\n"
  )

  out <- knitr::knit(
    text = src,
    quiet = TRUE
  )

  return(out)
}
