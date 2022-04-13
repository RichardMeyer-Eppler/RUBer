# Returns code chunk as list
code_chunk_list <- tpl_plot_figure(
  chunk_label = 1L,
  figure_caption = "Caption for figure 1",
  figure_height = 4.2
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
tpl_plot_figure(
  chunk_label = c(
    1L,
    2L
  ),
  figure_caption = c(
    "Caption for figure 1",
    "Caption for figure 2"
  ),
  figure_height = c(
    4.2,
    3.5
  )
)
