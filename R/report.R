

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
