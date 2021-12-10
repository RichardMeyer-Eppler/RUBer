#' Get author for report title page
#'
#' @param param_list List of parameters containing report_id, faculty_txt,
#'     subject_txt, subject_area_txt
#'
#' @return String
#' @export
#'
#' @examples
#' \dontrun{
#' get_author(df)
#' }
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
    "2018_FG" = "Sonderauswertung nach F\u00e4chergruppen"
  )

  return(author)
}

#' Get file name for automatic report generation
#'
#' @param param_list List of parameters containing report_id, report_nr,
#'     subject_txt, subject_area_txt
#'
#' @return String
#' @export
#'
#' @examples
#' \dontrun{
#' get_file_name(param_list)
#' }
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
    "2018_FG" = "Sonderauswertung nach F\u00e4chergruppen"
  )

  if(!is.null(file_name_suffix))  {
    file_name <- stringr::str_glue(
      "{file_name}_{file_name_suffix}"
    )
  }

  file_name <- clean_file_name(file_name)
  file_name <- paste0(file_name, ".docx")

  return(file_name)
}

#' Clean file name of special characters
#'
#' @param file_name Required string containing file name
#' @param char_limit Optional integer determining the maximum length of the file
#'     name in characters, defaults to 85.
#'
#' @return String
#'
#' @examples
#' clean_file_name(file_name = "Test_File-Name__X__")
clean_file_name <- function(file_name, char_limit = 85L) {
  datei_name <- stringr::str_replace_all(file_name, "[[:punct:]]", "_")
  datei_name <- stringr::str_replace_all(datei_name, "[[:space:]]", "_")
  datei_name <- stringr::str_replace_all(datei_name, "\u2013", "-")
  datei_name <- stringr::str_replace_all(datei_name, "\u2014", "-")
  datei_name <- stringr::str_replace_all(datei_name, "_{2,3}", "_")
  datei_name <- stringi::stri_trans_general(datei_name, "latin-ascii")
  if(nchar(datei_name) > char_limit) {
    datei_name <- stringr::str_sub(datei_name, 1, char_limit)
  }
  return(datei_name)
}

#' Filter data frame based on \code{report_nr} column
#'
#' @param df Data frame
#' @param report_nr Required integer indicating the report_nr
#'
#' @return Filtered data frame with data for the report nr
#' @export
#'
#' @examples
#' filter_report(df_fake, report_nr = 12)
filter_report <- function(df, report_nr) {
  p_report_nr <- report_nr

  filtered_df <- df %>%
    dplyr::filter(
      report_nr == p_report_nr &
        !figure_filter_flag
    )

  return(filtered_df)
}

#' Get file path for automatic report generation
#'
#' @param file_name Required string containing file name with file extension
#' @param file_directory String directory
#'
#' @return String file path
#' @export
#'
#' @examples
#' get_file_path(file_name = "test")
get_file_path <- function(
  file_directory,
  file_name
)  {
  here::here(
    file_directory,
    file_name
  )
}

#' Get title for report title page based on the column \code{report_nr}
#'
#' @param report_nr Required
#'
#' @return String
#' @export
#'
#' @examples
#' get_title(report_nr = 5)
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
#' @param p_df_stg Optional data frame with information on cases
#' @param report_nr Report number of the report
#' @param rmd_template Path to the R Markdown File for that report, defaults to
#'     \code{here::here("datenreport_new.Rmd")}
#' @param date Date of the report displayed on the title page, defaults to
#'     \code{format(Sys.Date(), format= "\%B \%Y")}.
#' @param output_directory Output directory for the rendered report, defaults to
#'     \code{here::here("output")}
#' @param output_filename Output filename for the rendered report, defaults to
#'     \code{p_df[[1, "file_name"]]}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' render_report(df, 12)
#' }
render_report <- function(
  p_df,
  p_df_stg = NULL,
  report_nr,
  rmd_template = here::here("datenreport_new.Rmd"),
  output_directory = NULL,
  output_filename = NULL,
  date = format(
    Sys.Date(),
    format= "%B %Y"
  )
) {
  df <- filter_report(p_df, report_nr)
  title <- df[[1, "report_title"]]
  author <- df[[1, "report_author"]]

  if(
    is.null(
      output_filename
    )
  ) {
    output_filename <- df[[1, "file_name"]]
  }

  if(
    is.null(
      output_directory
    )
  ) {
    output_directory <-   here::here(
      "output"
    )
  }

  file_path <- get_file_path(
    file_directory = output_directory,
    file_name = output_filename
  )

  # Create output directory, if it does not exist
	if(
	    !fs::dir_exists(
		    path = output_directory
		  )
	  ) {
	    fs::dir_create(
		    path = output_directory
	  )
	}

  # Apply transformation to data frame with student cases
  df_stg <- p_df_stg

  if(
    !is.null(
      p_df_stg
    )
  ) {
    df_stg <- Datenreport2022::get_stg_df(
      p_df_stg,
      report_nr = report_nr
    )
  }

  rmarkdown::render(
    rmd_template,
    params = list(
      p_report_nr = report_nr,
      p_title = title,
      p_author = author,
      p_date = date,
      p_df = df,
      p_df_stg = df_stg
    ),
    encoding = "UTF-8",
    output_file = file_path
    )
}

#' Sorts the report data frame
#'
#' Sorts report_nr, figure_sort, subject_id, subject_area_id, degree_sort,
#' time, variable_id, subject_group_id, value_id
#'
#' @param df Data frame with report data
#'
#' @return Sorted data frame
#' @export
#'
#' @examples
#' \dontrun{
#' sort_report(df)
#' }
sort_report <- function(df) {
  df_sorted <- df %>%
    dplyr::arrange(
      report_nr,
      figure_sort,
      subject_id,
      subject_area_id,
      degree_sort,
      time,
      variable_id,
      subject_group_id,
      value_id
    )

  return(df_sorted)
}
