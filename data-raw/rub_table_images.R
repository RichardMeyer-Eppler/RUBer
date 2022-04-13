# This script creates the figures in "man/figures" for each flextable function

# Libraries --------------------------------------------------------------------

## Github
# remotes::install_github("coolbutuseless/optout") # for PNG compression
# Compression library pngquant needs to be installed separately, add to PATH
# https://pngquant.org/

library(flextable)
library(here)
library(optout)
library(purrr)
library(RUBer)

# Create figures for inst/examples ---------------------------------------------

create_image <- function(
  file_name
) {
  source_file <- paste0(
    "inst/examples/",
    file_name,
    ".R"
  )

  output_file <- here::here(
    "man",
    "figures",
    paste0(
      file_name,
      ".png"
    )
  )

  flextable::save_as_image(
    source(
      source_file,
      encoding = "UTF8"
    )[[1]],
    path = output_file
  )

  optout::pngquant(
    infile = output_file,
    outfile = output_file,
    verbosity = 1
  )
}

table_function_names <- c(
  "rub_table_stg",
  "rub_table_eb",
  "rub_table_vb",
  "rub_table_ab",
  "rub_table_programs",
  "rub_table_item",
  "rub_table_metrics",
  "rub_table_excluded_programs",
  "rub_table_included_programs"
)

purrr::walk(
  table_function_names,
  create_image
)
