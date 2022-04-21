#' Wrap render_report in purrr::safely when package is loaded
safely_render_report <- function(...) "dummy"


.onLoad <- function(lib, pkg) {
  # See https://purrr.tidyverse.org/reference/faq-adverbs-export.html
  safely_render_report <<- purrr::safely(
    render_report,
    quiet = FALSE
  )
}
