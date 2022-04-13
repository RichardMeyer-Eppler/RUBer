# Returns code chunk as list
code_chunk_list <- tpl_get_figure_df(
  chunk_label = 1L,
  function_params = "df, figure_count == 1L"
)

# Unlist
code_chunk_vector <- unlist(
  code_chunk_list
)

# Display code chunks as they will be written to the Rmd
writeLines(
  code_chunk_vector
)

# The function is vectorized, so you can do this:
tpl_get_figure_df(
  chunk_label = c(
    1L,
    2L
  ),
  function_params = c(
    "df, figure_count == 1L",
    "df, figure_count == 2L"
  )
)
