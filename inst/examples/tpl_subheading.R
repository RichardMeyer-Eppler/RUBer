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
