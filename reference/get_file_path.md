# Get file path for automatic report generation

Get file path for automatic report generation

## Usage

``` r
get_file_path(
  file_directory = "output",
  file_name = fs::path_file(fs::file_temp(ext = ".docx"))
)
```

## Arguments

- file_directory:

  Optional string directory relative to the project folder

- file_name:

  Required string containing file name with file extension

## Value

String file path

## Examples

``` r
get_file_path(file_name = "test.docx")
#> output/test.docx
```
