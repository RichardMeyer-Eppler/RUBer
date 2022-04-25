

#' Filter data frame based on \code{report_nr}
#'
#' @param df Data frame
#' @param report_nr Required integer indicating the report_nr
#'
#' @return Filtered data frame with data for the report nr
#' @export
#'
#' @examples
#' filter_report(df_example, report_nr = 12)
filter_report <- function(df, report_nr) {

  filtered_df <- df %>%
    dplyr::filter(
      report_nr == !!report_nr
    )

  return(filtered_df)
}

#' Get file path for automatic report generation
#'
#' @param file_name Required string containing file name with file extension
#' @param file_directory Optional string directory relative to the project folder
#'
#' @return String file path
#' @export
#'
#' @examples
#' get_file_path(file_name = "test.docx")
get_file_path <- function(
  file_directory = "output",
  file_name = fs::path_file(
    tempfile(
      fileext = ".docx"
    )
  )
)  {
  here::here(
    file_directory,
    file_name
  )
}

#' Get all unique values of `report_nr` for a `report_type_id`
#'
#' @param df Data frame with columns `report_nr` and `report_type_id`
#' @param report_type_id Any of `c("STG", "M_ED", "MED", "FGR", "SZMA")`
#'
#' @return Integer vector
#' @export
#'
#' @importFrom rlang .env
#'
#' @examples
#' get_report_nr_by_id(df = df_example, report_type_id = "FGR")
get_report_nr_by_id <- function(
  df,
  report_type_id
) {

  report_nr <- df %>%
    dplyr::filter(
      report_type_id %in% .env[["report_type_id"]]
    ) %>%
    dplyr::pull(
      report_nr
    ) %>%
    unique(
      .
    )

  return(report_nr)
}

# #' Call `render_report` for each report in `RUBer::df_report`
# #'
# #' @param df
# #' @param df_report
# #' @param report_nr
# #' @param report_type_id
# #'
# #' @return Side effects
# #' @export
# #'
# #' @importFrom rlang .env
# #' @examples
# #' render_all_reports(report_nr = 6L)
# render_all_reports <- function(
#   df = RUBer::df_example,
#   df_report = RUBer::df_report,
#   report_nr = NULL,
#   report_type_id = NULL
# ) {
#
#   if(
#     !is.null(report_nr)
#   ) {
#     df <- df %>%
#       dplyr::filter(
#         report_nr %in% .env[["report_nr"]]
#       )
#
#     df_report <- df_report %>%
#       dplyr::filter(
#         report_nr %in% .env[["report_nr"]]
#       )
#   }
#
#   if(
#     !is.null(report_type_id)
#   ) {
#     df <- df %>%
#       dplyr::filter(
#         report_type_id %in% .env[["report_type_id"]]
#       )
#
#     df_report <- df_report %>%
#       dplyr::filter(
#         report_type_id %in% .env[["report_type_id"]]
#       )
#   }
#
#   reports <- df_report[["report_nr"]]
#   titles <- df_report[["report_title"]]
#   authors <- df_report[["report_author"]]
#   output_directories <- df_report[["output_path"]]
#   output_filenames <- df_report[["file_name"]]
#
#   df_list <- purrr::map(
#     .x = report_nr,
#     filter_report,
#     df = df
#   )
#
#   rmd_template <- fs::path_package(
#     package = "RUBer",
#     "rmarkdown",
#     "templates",
#     "datenreport-2022",
#     "skeleton",
#     "skeleton.Rmd"
#   )
#
#   date <- format(Sys.Date(), format = "%B %Y")
#
#
#   path_figure_template <- fs::file_temp(
#     pattern = "figure_template_",
#     ext = ".Rmd"
#   )
#
#   purrr::pwalk(
#     list(
#       p_df = df_list,
#       report_nr = reports,
#       output_directory = output_directories,
#       output_filename = output_filenames,
#       title = titles,
#       author = authors,
#       path_figure_template = path_figure_template()
#     ),
#     safely_render_report,
#     rmd_template = rmd_template,
#     date = date,
#     post_process = TRUE,
#     path_figure_template = path_figure_template
#   )
#
#   return(
#     invisible(
#       df
#     )
#   )
# }

#' Render a single parametric report as Word file
#'
#' @description
#'
#' The `render_report` function is called once for each report to be created. `render_report_safely`
#' wraps `render_report` in `purrr::safely`.
#'
#' @param p_df Data frame containing the data for all reports
#' @param p_df_stg Optional data frame with information on cases
#' @param report_nr Report number of the report
#' @param rmd_template Path to the R Markdown File for that report, defaults to Datenreport 2022 template from RUBer package
#' @param date Date of the report displayed on the title page, defaults to
#'     \code{format(Sys.Date(), format= "\%B \%Y")}.
#' @param output_directory Output directory for the rendered report, defaults to
#'     \code{here::here("output")}
#' @param output_filename Output filename for the rendered report, defaults to
#'     \code{p_df[[1, "file_name"]]}
#' @param post_process Boolean, whether \code{post_process} gets called on the
#'     output file
#' @param title Character, title for the title page
#' @param author Character, author for the title page
#' @param font_file Character, font file to use in all plots, defaults to
#'     "RubFlama-Regular.ttf"
#' @param path_figure_template Character, file path to write the dynamically generated figure chunks
#'     to file (useful for debugging purposes). Defaults to
#'     `fs::file_temp(pattern = "figure_template_", ext = ".Rmd")`
#' @param ... Arguments passed on to `render_report`
#' @return Invisibly returns `p_df`
#' @export
#'
#' @importFrom officedown rdocx_document
#' @importFrom showtext showtext_auto
#'
#' @example inst/examples/render_report.R
render_report <- function(
  p_df = filter_report(RUBer::df_example, 6L),
  p_df_stg = NULL,
  report_nr = 6L,
  rmd_template = system.file(
    "rmarkdown",
    "templates",
    "datenreport-2022",
    "skeleton",
    "skeleton.Rmd",
    package = "RUBer"
  ),
  output_directory = fs::path_temp(),
  output_filename = fs::path_file(
    fs::file_temp(
      pattern = "report",
      ext = ".docx"
    )
  ),
  title = "Title",
  author = "Author",
  date = format(
    Sys.Date(),
    format= "%B %Y"
  ),
  font_file = "RubFlama-Regular.ttf",
  path_figure_template = fs::file_temp(
    pattern = "figure_template_",
    ext = ".Rmd"
  ),
  post_process = TRUE
) {

  df <- p_df

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

  rmarkdown::render(
    input = rmd_template,
    params = list(
      p_report_nr = report_nr,
      p_title = title,
      p_author = author,
      p_date = date,
      p_df = df,
      p_df_stg = p_df_stg,
      p_path_figure_template = path_figure_template,
      p_font_file = font_file
    ),
    encoding = "UTF-8",
    output_file = file_path
  )

  if(post_process) {

    post_process(
      old_path = file_path,
      new_path = file_path,
      overwrite = TRUE
    )
  }

  return(
    invisible(
      p_df
    )
  )

}

#' @rdname render_report
render_report_safely <- function(...) "dummy"

#' Post process Word file created with officedown
#'
#' @param old_path String with path to Word file created by officedown
#' @param new_path New path for edited Word file
#' @param overwrite Boolean, overwrite files if they exist. If this is `FALSE`
#'     and the file exists an error will be thrown.
#'
#' @return side effects
#' @keywords internal
#'
#' @examples
post_process <- function(
  old_path,
  new_path,
  overwrite = FALSE
) {

  doc <- officer::read_docx(
    old_path
  )

  # Insert tabs in all captions
  doc_placeholder <- replace_placeholder(
    doc = doc
  )

  # Replace toc levels for list of figures
  doc_img <- replace_toc(
    doc = doc_placeholder,
    style = 'Image Caption',
    new_level = 5
  )

  # Replace toc levels for list of figures
  doc_tbl <- replace_toc(
    doc = doc_img,
    style = 'Table Caption',
    new_level = 6
  )

  # Create first page header
  doc_header <- replace_first_page_header(
    doc_tbl
  )

  output_directory <- fs::path_dir(
    new_path
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

  if(
    fs::file_exists(
      new_path
    ) &
    !overwrite
  ) {
    rlang::abort(
      message = glue::glue("{new_path} already exists and overwrite = FALSE")
    )
  } else {
    print(
      doc_header,
      target = new_path
    )
  }
}

#' Replace placeholder strings in docx document
#'
#' @param doc an rdocx object
#' @param placeholder_text Character with the placeholder to be replaced, defaults to "PLACEHOLDER_TAB"
#' @param replacement_text Character with replacement text, defaults to "\\t"
#'
#' @return Side effects
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' replace_placeholder(
#'    path = "test.docx",
#'    new_path = "test_replaced.docx"
#' )
#' }
replace_placeholder <- function(
  doc,
  placeholder_text = "PLACEHOLDER_TAB",
  replacement_text = "\t"
) {

  report_replaced <- officer::body_replace_all_text(
    x = doc,
    old_value = placeholder_text,
    new_value = replacement_text,
    only_at_cursor = FALSE,
    ignore.case = TRUE
  )

  return(report_replaced)

}

#' Replace level of TOC field
#'
#' @param doc an rdocx object
#' @param style text, the style referenced by the TOC field
#' @param new_level numeric, the new level for the TOC field
#'
#' @return A modified rdocx object
#' @keywords internal
#'
#' @examples
replace_toc <- function(
  doc,
  style,
  new_level
) {

  xml <- officer::docx_body_xml(
    doc
  )

  search_txt <- glue::glue(
    '//w:instrText[contains(.,"{style}")]'
  )

  toc_node <- xml2::xml_find_first(
    xml,
    search_txt
  )

  replacement_txt <- glue::glue(
    'TOC \\h \\z \\t \"{style};{new_level}\"'
  )

  xml2::xml_set_text(
    toc_node,
    replacement_txt
  )

  return(doc)

}

#' Create header with option "different first page" enabled
#'
#' @param doc an rdocx object
#'
#' @return A modified rdocx object
#' @keywords internal
#'
#' @examples
replace_first_page_header <- function(
  doc
) {

  relationships <- officer::docx_body_relationship(
    doc
  )

  df_relationships <- relationships$get_data()

  default_header_id <- df_relationships %>%
    dplyr::filter(
      .data[["target"]] == "header2.xml"
    ) %>%
    dplyr::pull(
      .data[["id"]]
    )

  default_footer_id <- df_relationships %>%
    dplyr::filter(
      .data[["target"]] == "footer2.xml"
    ) %>%
    dplyr::pull(
      .data[["id"]]
    )

  first_page_header_id <- df_relationships %>%
    dplyr::filter(
      .data[["target"]] == "header3.xml"
    ) %>%
    dplyr::pull(
      .data[["id"]]
    )

  generate_xml <- function(
    default_header_id,
    default_footer_id,
    first_page_header_id
  ) {

    node_txt <- glue::glue(
      '<w:sectPr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" w:rsidR="009C0CC2" w:rsidSect="002C6CD0"><w:headerReference w:type="default" r:id="{default_header_id}"/><w:footerReference w:type="default" r:id="{default_footer_id}"/><w:headerReference w:type="first" r:id="{first_page_header_id}"/><w:type w:val="continuous"/><w:pgSz w:w="11952" w:h="16848"/><w:pgMar w:top="1417" w:right="1134" w:bottom="1134" w:left="850" w:header="720" w:footer="720" w:gutter="0"/><w:cols w:space="720"/><w:titlePg/><w:docGrid w:linePitch="326"/></w:sectPr>'
    )

    node_xml <- xml2::read_xml(
      node_txt
    )

    return(node_xml)
  }

  old_node <- xml2::xml_find_first(
    officer::docx_body_xml(
      doc
    ),
    "w:body/w:sectPr"
  )

  xml2::xml_replace(
    old_node,
    generate_xml(
      default_header_id,
      default_footer_id,
      first_page_header_id
    )
  )

  return(doc)

}
