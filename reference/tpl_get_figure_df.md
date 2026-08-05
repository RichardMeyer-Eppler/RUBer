# Returns a character vector for the code chunk to retrieve the figure data frame

Returns a character vector for the code chunk to retrieve the figure
data frame

## Usage

``` r
tpl_get_figure_df(
  chunk_label,
  function_params,
  function_call = "dplyr::filter"
)
```

## Arguments

- chunk_label:

  Character, chunk label

- function_params:

  Character

- function_call:

  Character, defaults to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)

## Value

List of character vectors with chunk texts

## Examples

``` r
# Returns code chunk as list
code_chunk_list <- tpl_get_figure_df(
  chunk_label = 1L,
  function_params = "df, figure_nr == 1L"
)

# Unlist
code_chunk_vector <- unlist(
  code_chunk_list
)

# Display code chunks as they will be written to the Rmd
writeLines(
  code_chunk_vector
)
#> 
#> ```{r prep-fig-1, include = FALSE}
#> df_fig <- dplyr::filter(df, figure_nr == 1L)
#> ```

# The function is vectorized, so you can do this:
tpl_get_figure_df(
  chunk_label = c(
    1L,
    2L
  ),
  function_params = c(
    "df, figure_nr == 1L",
    "df, figure_nr == 2L"
  )
)
#> [[1]]
#> [1] "\n```{r prep-fig-1, include = FALSE}"        
#> [2] "df_fig <- dplyr::filter(df, figure_nr == 1L)"
#> [3] "```"                                         
#> 
#> [[2]]
#> [1] "\n```{r prep-fig-2, include = FALSE}"        
#> [2] "df_fig <- dplyr::filter(df, figure_nr == 2L)"
#> [3] "```"                                         
#> 
```
