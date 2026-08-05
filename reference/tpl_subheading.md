# Returns a character vector for the code chunk to insert a subheading

Returns a character vector for the code chunk to insert a subheading

## Usage

``` r
tpl_subheading(chunk_label, subheading, level = 2L)
```

## Arguments

- chunk_label:

  Character, chunk label

- subheading:

  Character, subheading text

- level:

  Integer, level of the heading, defaults to `2L`

## Value

List of character vectors with chunk texts

## Examples

``` r
# Returns code chunk as list
code_chunk_list <- tpl_subheading(
  chunk_label = 1L,
  subheading = "Subsection 1"
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
#> ```{r subheading-fig-1, results= 'asis'}
#> cat(paste("##", "Subsection 1"))
#> ```

# The function is vectorized, so you can do this:
tpl_subheading(
  chunk_label = c(
    1L,
    2L
  ),
  subheading = c(
    "Subsection 1",
    "Subsection 2"
  )
)
#> [[1]]
#> [[1]][[1]]
#> [1] "\n```{r subheading-fig-1, results= 'asis'}"
#> [2] "cat(paste(\"##\", \"Subsection 1\"))"      
#> [3] "```"                                       
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "\n```{r subheading-fig-2, results= 'asis'}"
#> [2] "cat(paste(\"##\", \"Subsection 2\"))"      
#> [3] "```"                                       
#> 
#> 
```
