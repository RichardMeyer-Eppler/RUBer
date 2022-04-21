.onLoad <- function(lib, pkg) {
  # See https://purrr.tidyverse.org/reference/faq-adverbs-export.html
  render_report_safely <<- purrr::safely(
    render_report,
    quiet = FALSE
  )
}
