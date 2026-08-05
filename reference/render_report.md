# Render a single parametric report as Word file

The `render_report` function is called once for each report to be
created. `render_report_safely` wraps `render_report` in
[`purrr::safely`](https://purrr.tidyverse.org/reference/safely.html).

## Usage

``` r
render_report(
  p_df = filter_report(df = RUBer::df_example, report_nr = 6L),
  p_df_stg = NULL,
  report_nr = 6L,
  rmd_template = fs::path_package(package = "RUBer", "rmarkdown", "templates",
    "datenreport-2022", "skeleton", "skeleton.Rmd"),
  output_directory = fs::path_temp(),
  output_filename = fs::path_file(fs::file_temp(pattern = "RUBer_report_", ext =
    ".docx")),
  title = "Title",
  author = "Author",
  date = format(Sys.Date(), format = "%B %Y"),
  font_file = "RubFlama-Regular.ttf",
  path_figure_template = fs::file_temp(pattern = "figure_template_", ext = ".Rmd"),
  post_process = TRUE,
  quiet = FALSE
)

render_report_safely(...)
```

## Arguments

- p_df:

  Data frame containing the data for all reports

- p_df_stg:

  Optional data frame with information on cases

- report_nr:

  Report number of the report

- rmd_template:

  Path to the R Markdown File for that report, defaults to Datenreport
  2022 template from RUBer package

- output_directory:

  Output directory for the rendered report, defaults to
  `here::here("output")`

- output_filename:

  Output filename for the rendered report, defaults to
  `p_df[[1, "file_name"]]`

- title:

  Character, title for the title page

- author:

  Character, author for the title page

- date:

  Date of the report displayed on the title page, defaults to
  `format(Sys.Date(), format= "%B %Y")`.

- font_file:

  Character, font file to use in all plots, defaults to
  "RubFlama-Regular.ttf"

- path_figure_template:

  Character, file path to write the dynamically generated figure chunks
  to file (useful for debugging purposes). Defaults to
  `fs::file_temp(pattern = "figure_template_", ext = ".Rmd")`

- post_process:

  Boolean, whether `post_process` gets called on the output file

- quiet:

  An option to suppress printing during rendering from knitr, pandoc
  command line and others. Passed on to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

- ...:

  Arguments passed on to `render_report`

## Value

Invisibly returns `p_df`

## Examples

``` r
# Check if pandoc is available in the minimum required version
pandoc_available <- rmarkdown::pandoc_available(
  version = "2.0",
  error = FALSE
)

if(pandoc_available){

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
}
#> 
#> 
#> processing file: skeleton.Rmd
#> 1/57                                         
#> 2/57 [setup]                                 
#> 3/57                                         
#> 4/57 [data]                                  
#> 5/57                                         
#> 6/57 [figure_template]                       
#> 7/57                                         
#> 8/57 [unnamed-chunk-1]                       
#> 9/57                                         
#> 10/57 [unnamed-chunk-2]                       
#> 11/57                                         
#> 12/57 [pagebreak-metrics-before]              
#> 13/57                                         
#> 14/57 [metrics]                               
#> 15/57                                         
#> 16/57 [stg-cases-eb]                          
#> 17/57                                         
#> 18/57 [stg-cases-vb]                          
#> 19/57                                         
#> 20/57 [stg-cases-ab]                          
#> 21/57                                         
#> 22/57 [funded-projects-table]                 
#> 23/57                                         
#> 24/57 [showtext-on]                           
#> 25/57                                         
#> 26/57 [create-figures]                        
#> 27/57                                         
#> 28/57 [paste-figures]                         
#> 29/57                                         
#> 30/57 [showtext-off]                          
#> 31/57                                         
#> 32/57 [items-child]                           
#> 
#> 
#> processing file: ./items.Rmd
#> 1/1
#> 33/57                                         
#> 34/57 [pagebreak-items-table]                 
#> 35/57                                         
#> 36/57 [items-table]                           
#> 37/57                                         
#> 38/57 [studieneingang]                        
#> 39/57                                         
#> 40/57 [pagebreak-eb]                          
#> 41/57                                         
#> 42/57 [studienverlauf]                        
#> 43/57                                         
#> 44/57 [pagebreak-vb]                          
#> 45/57                                         
#> 46/57 [studienabschluss]                      
#> 47/57                                         
#> 48/57 [pagebreak-ab]                          
#> 49/57                                         
#> 50/57 [studiengaenge]                         
#> Error in data.frame(i = i, j = j): arguments imply differing number of rows: 4, 3
#> Error: object 'ft_included_programs' not found
#> 51/57                                         
#> 52/57 [pagebreak-studiengaenge]               
#> 53/57                                         
#> 54/57 [studiengaenge-ausgeschlossen]          
#> 55/57                                         
#> 56/57 [pagebreak-studiengaenge-ausgeschlossen]
#> 57/57                                         
#> output file: skeleton.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS skeleton.knit.md --to docx --from markdown+autolink_bare_uris+tex_math_single_backslash --output /tmp/Rtmp7CMMI3/RUBer_report_1b1963d349d4.docx --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --table-of-contents --toc-depth 2 --syntax-highlighting tango --reference-doc /home/runner/work/_temp/Library/RUBer/rmarkdown/templates/datenreport-2022/skeleton/rub_reference_2021.docx --extract-media /tmp/Rtmp7CMMI3/RUBer_report_1b1963d349d4_files --citeproc 
#> 
#> Output created: /tmp/Rtmp7CMMI3/RUBer_report_1b1963d349d4.docx
#> ℹ Report "/tmp/Rtmp7CMMI3/RUBer_report_1b1963d349d4.docx" was written successfully.
```
