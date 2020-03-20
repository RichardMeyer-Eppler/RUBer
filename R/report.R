#' Get author for report title page
#'
#' @param df
#'
#' @return String
#' @export
#'
#' @examples
#' get_author(df)
get_author <- function(param_list)  {
  report_id <- param_list[["report_id"]]
  faculty_txt <- param_list[["faculty_txt"]]
  subject_txt <- param_list[["subject_txt"]]
  subject_area_txt <- param_list[["subject_area_txt"]]

  author <- switch(
    report_id,
    "2018_LE" = stringr::str_glue(
      "{faculty_txt} - Lehreinheit {subject_area_txt}"
    ),
    "2018_LE_530" = stringr::str_glue(
      "{faculty_txt} - Lehreinheit {subject_area_txt}"
    ),
    "2018_LE_802" = faculty_txt,
    "2018_M_ED" = "Master of Education",
    "2018_STG" = stringr::str_glue(
      "{faculty_txt} - Fach {subject_txt}"),
    "2018_FG" = "Sonderauswertung nach Fächergruppen"
  )

  return(author)
}

#' Get file name for automatic report generation
#'
#' @param param_list
#'
#' @return String
#' @export
#'
#' @examples
#' get_file_name(param_list)
get_file_name <- function(param_list) {
  report_id <- param_list[["report_id"]]
  report_nr <- param_list[["report_nr"]]
  subject_txt <- param_list[["subject_txt"]]
  subject_area_txt <- param_list[["subject_area_txt"]]

  year <- format(
    Sys.Date(),
    format= "%Y"
  )
  file_name <- stringr::str_glue(
    "RUB_Datenreport_Nr{report_nr}_{year}"
  )

  file_name_suffix <- switch(
    report_id,
    "2018_LE" = subject_area_txt,
    "2018_LE_530" = subject_area_txt,
    "2018_LE_802" = "Medizin",
    "2018_M_ED" = "MEd",
    "2018_STG" = subject_txt,
    "2018_FG" = "Sonderauswertung nach Fächergruppen"
  )

  if(!is.null(file_name_suffix))  {
    file_name <- stringr::str_glue(
      "{file_name}_{file_name_suffix}"
    )
  }

  file_name <- RUBer::clean_file_name(file_name)
  file_name <- paste0(file_name, ".docx")

  return(file_name)
}

#' Clean file name of special characters
#'
#' @param file_name
#'
#' @return String
#' @export
#'
#' @examples
#' clean_file_name("Test_File-Name__X__")
clean_file_name <- function(file_name) {
  datei_name <- stringr::str_replace_all(file_name, "[[:punct:]]", "_")
  datei_name <- stringr::str_replace_all(datei_name, "[[:space:]]", "_")
  datei_name <- stringr::str_replace_all(datei_name, "–", "-")
  datei_name <- stringr::str_replace_all(datei_name, "_{2,3}", "_")
  datei_name <- stringi::stri_trans_general(datei_name, "latin-ascii")
  if(nchar(datei_name) > 85) {
    datei_name <- stringr::str_sub(datei_name, 1, 85)
  }
  return(datei_name)
}

#' Get file path for automatic report generation
#'
#' @param file_name with file extension
#'
#' @return String file path
#' @export
#'
#' @examples
#' get_file_path("test")
get_file_path <- function(file_name)  {
  here::here(
    "output",
    file_name
  )
}

#' Get title for report title page based on the column "report_nr"
#'
#' @param report_nr
#'
#' @return String
#' @export
#'
#' @examples
#' get_title(df)
get_title <- function(report_nr) {
  if (!is.na(report_nr)) {
    title <- paste("Datenreport Nr.", report_nr)
  } else  {
    title <- NA_character_
  }

  return(title)
}

#' Render report
#'
#' @param p_df Data frame containing the data for all reports
#' @param report_nr Report number of the report
#' @param p_rmd_template Path to the R Markdown File for that report
#' @param date Date of the report displayed on the title page, defaults to month
#'     and year.
#'
#' @export
#'
#' @examples
#' render_report(df, 12)
render_report <- function(p_df, report_nr,
                          rmd_template = here::here("datenreport_new.Rmd"),
                          date = format(Sys.Date(), format= "%B %Y")) {
  df <- RUBer::filter_report(p_df, report_nr)
  title <- df[[1, "report_title"]]
  author <- df[[1, "report_author"]]
  file_name <- df[[1, "file_name"]]
  file_path <- RUBer::get_file_path(file_name)

  rmarkdown::render(rmd_template, params = list(
    p_title = title,
    p_author = author,
    p_date = date,
    p_df = df
  ),
  encoding = "UTF-8",
  output_file = file_path)
}
