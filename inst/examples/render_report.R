run_ok <- rmarkdown::pandoc_available() &&
  rmarkdown::pandoc_version() >= numeric_version("2.0")

if(run_ok){

  output_directory <- fs::path_temp()

  output_filename <- fs::path_file(
    fs::file_temp(
      pattern = "report",
      ext = ".docx"
    )
  )

  render_report(
    output_directory = output_directory,
    output_filename = output_filename
  )

  path_report_docx <- get_file_path(
    output_directory,
    output_filename
  )

  if(
    file.exists(
      path_report_docx
    )
  ){
    message(
      "File ",
      path_report_docx,
      " was written successfully."
    )
  }
}
