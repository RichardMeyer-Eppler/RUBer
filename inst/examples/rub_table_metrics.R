# Path to skeleton files
path_skeleton <- fs::path_package(
  package = "RUBer",
  "rmarkdown",
  "templates",
  "datenreport-2022",
  "skeleton"
)

# Read csv file
df_metrics <- read.csv(
  fs::path(
    path_skeleton,
    "metrics_overview.csv"
  ),
  encoding = "UTF-8"
)

# Extract vectors from data frame, construct full file paths
metrics_text <- df_metrics[["metrics_text"]]
metrics_images <- as.character(
  fs::path(
    path_skeleton,
    df_metrics[["metrics_images"]]
  )
)

# For presentation purposes, the data is split into six columns
df_metrics_table <- tibble::tribble(
  ~col1, ~col2, ~col3, ~col4, ~col5, ~col6,
  metrics_images[1], metrics_text[1], metrics_text[1],
  metrics_text[1], NA_character_, NA_character_,
  metrics_text[2], metrics_text[2], metrics_text[2],
  metrics_text[2], metrics_text[2], metrics_images[2],
  metrics_images[3], metrics_text[3], metrics_text[3],
  metrics_text[3], NA_character_, NA_character_,
  NA_character_, NA_character_, metrics_text[4],
  metrics_text[4], metrics_text[4], metrics_images[4],
  metrics_images[5], metrics_text[5], metrics_text[5],
  metrics_text[5], metrics_text[5], NA_character_,
  NA_character_, metrics_text[6], metrics_text[6],
  metrics_text[6], metrics_text[6], metrics_images[6]
)

# Function call
rub_table_metrics(
  df_metrics_table
)
