# Returns a character vector for the code chunk to insert a heading

Returns a character vector for the code chunk to insert a heading

## Usage

``` r
tpl_heading(chunk_label, heading, level = 1L)
```

## Arguments

- chunk_label:

  Character, chunk label

- heading:

  Character, heading text

- level:

  Integer, level of the heading, defaults to `1L`

## Value

List of character vectors with chunk texts

## Examples

``` r
# Returns code chunk as list
code_chunk_list <- tpl_heading(
  chunk_label = 1L,
  heading = "Section 1"
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
#> ```{r heading-fig-1, results= 'asis'}
#> cat(paste("#", "Section 1"))
#> ```

# The function is vectorized, so you can do this:
tpl_heading(
  chunk_label = c(
    1L,
    2L
  ),
  heading = c(
    "Section 1",
    "Section 2"
  )
)
#> [[1]]
#> [[1]][[1]]
#> [1] "\n```{r heading-fig-1, results= 'asis'}"
#> [2] "cat(paste(\"#\", \"Section 1\"))"       
#> [3] "```"                                    
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "\n```{r heading-fig-2, results= 'asis'}"
#> [2] "cat(paste(\"#\", \"Section 2\"))"       
#> [3] "```"                                    
#> 
#> 
```
