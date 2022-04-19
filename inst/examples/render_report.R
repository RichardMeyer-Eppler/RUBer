# Check if pandoc is available in the minimum required version
run_ok <- rmarkdown::pandoc_available() &&
  rmarkdown::pandoc_version() >= numeric_version("2.0")

if(run_ok){

  # Get paths for skeleton.Rmd
  skeleton_location <- fs::path_package(
    package = "RUBer",
    "rmarkdown",
    "templates",
    "datenreport-2022",
    "skeleton",
    "skeleton.Rmd"
  )

  # Session based output directory
  output_directory <- fs::path_temp()

  # File name for the report output file
  output_filename <- fs::path_file(
    fs::file_temp(
      pattern = "RUBer_report_",
      ext = ".docx"
    )
  )

  # Full path to output file
  path_report_docx <- fs::path(
    output_directory,
    output_filename
  )

  # Create output directory
  fs::dir_create(
    output_directory
  )

  # Copy all files in the skeleton folder to the output directory
  fs::file_copy(
    path = fs::dir_ls(
      fs::path_dir(
        skeleton_location
      )
    ),
    new_path = output_directory
  )

  # Render the report in the temporary output directory
  render_report(
    rmd_template = skeleton_location,
    output_directory = output_directory,
    output_filename = output_filename
  )

  if(
    fs::file_exists(
      path_report_docx
    )
  ){
    rlang::inform(
      message = c(
        "i" = glue::glue(
          'File "{path_report_docx}" was written successfully.'
        )
      )
    )
  }
}
