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
