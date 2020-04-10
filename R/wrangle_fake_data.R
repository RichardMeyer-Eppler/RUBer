#' Get data frame with generated fake values
#'
#' @param df Data frame
#' @param report_id_group_level A vector of report_ids, indicating which reports
#'    do not have reference group comparisons plotted for each figure.
#'
#' @return Data frame with fake values for \code{axis_label},
#'    \code{value_n_total} and \code{value_percentage}.
#' @export
#'
#' @examples
#' \dontrun{
#' get_fake_df(df, report_id_group_level = c(62, 64))
#' }
get_fake_df <- function(df, report_id_group_level = NULL) {
  df_t1_t4 <- get_fake_data_type_1_4(df)
  df_t2 <- get_fake_data_type_2(df)
  df_t3 <- get_fake_data_type_3(df, report_id_group_level)

  df <- df_t1_t4 %>%
    dplyr::bind_rows(df_t2) %>%
    dplyr::bind_rows(df_t3)

  df <- df %>%
    # Figure type 3 have fake values in the axis label
    dplyr::mutate(
      axis_label_fake = dplyr::if_else(
        figure_type_id %in% c(1,2,4),
        axis_label,
        axis_label_fake
      )
    ) %>%
    dplyr::select(
      -value_n_total,
      -value_percentage,
      -axis_label
    ) %>%
    dplyr::rename(
      axis_label = axis_label_fake,
      value_n_total = value_n_total_fake,
      value_percentage = value_percentage_fake
    )

  return(df)
}

#' Get data frame with fake values for figure types 1 and 4
#'
#' @param df Data frame
#'
#' @return Data frame with fake values for value_n_total
#' @export
#'
#' @examples
#' \dontrun{
#' get_fake_data_type_1_4(df)
#' }
get_fake_data_type_1_4 <- function(df) {
  df_type_1_4 <- dplyr::filter(
    df,
    figure_type_id %in% c(1, 4)
  )

  df_type_1_4_fake <- df_type_1_4 %>%
    dplyr::add_count(
      report_nr,
      figure_count,
      figure_type_id,
      degree_id,
      name = "time_length"
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      figure_type_id,
      degree_id,
      time_length
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      fake_values_1 = dplyr::if_else(
        figure_type_id == 1,
        purrr::pmap(
          list(
            figure_type_id,
            time_length
          ),
          get_fake_values_type_1_4
        ),
        list(0)
      ),
      median_fake_values_1 = stats::median(
        unlist(
          fake_values_1
        )
      )
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::mutate(
      base_value = cumsum(
        median_fake_values_1
      ),
      fake_values_4 = dplyr::if_else(
        figure_type_id == 4,
        purrr::pmap(
          list(
            figure_type_id,
            time_length,
            base_value
          ),
          get_fake_values_type_1_4
        ),
        list(0)
      )
    ) %>%
    tidyr::unnest(
      cols = data
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_id
    ) %>%
    dplyr::mutate(
      # Helper index for retrieving element from vector of fake values
      value_counter = dplyr::row_number(),
      value_n_total_fake = dplyr::if_else(
        figure_type_id == 1,
        purrr::map2_dbl(
          fake_values_1,
          value_counter,
          ~ .x[.y]
        ),
        purrr::map2_dbl(
          fake_values_4,
          value_counter,
          ~ .x[.y]
        ),
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -fake_values_1,
      -median_fake_values_1,
      -base_value,
      -fake_values_4,
      -value_counter,
      -time_length
    )

  return(df_type_1_4_fake)
}

#' Create fake values for figure type 2
#'
#' @param df Data frame
#'
#' @return Data frame with fake values for figure type 2
#' @export
#'
#' @examples
#' \dontrun{
#' get_fake_data_type_2(df)
#' }
get_fake_data_type_2 <- function(df) {
  df_type_2 <- dplyr::filter(
    df,
    figure_type_id == 2
  )

  df_type_2_fake <- df_type_2 %>%
    # Number of values for each variable
    dplyr::add_count(
      report_nr,
      figure_count,
      degree_id,
      variable_id,
      name = "fractions"
    ) %>%
    # Number of times each value appears
    dplyr::add_count(
      report_nr,
      figure_count,
      degree_id,
      value_id,
      name = "periods"
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_id,
      fractions,
      periods
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      value_percentage_fake = purrr::map2(
        fractions,
        periods,
        get_fake_values_type_2
      )
    ) %>%
    tidyr::unnest(
      cols = data
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::mutate(
      # Helper index for retrieving element from vector of fake values
      value_counter = dplyr::row_number(),
      value_percentage_fake = purrr::map2_dbl(
        value_percentage_fake,
        value_counter,
        ~ .x[.y]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -value_counter,
      -fractions,
      -periods
    )

  return(df_type_2_fake)
}


#' Create fake values for figure type 3
#'
#' @param df Data frame
#' @param report_id_group_level A vector of report_ids, indicating which reports
#'     do not have hierarchical reference group comparisons
#'
#' @return Data frame with fake values for figure type 3
#' @export
#'
#' @examples
#' \dontrun{
#' get_fake_data_type_3(df, report_id_group_level = c(62, 64))
#' }
get_fake_data_type_3 <- function(df, report_id_group_level = NULL) {
  df_type_3 <- dplyr::filter(
    df,
    figure_type_id == 3
  )

  pattern <- "\\([^()]+\\)" # Replaces string in round brackets

  df_type3_reference_group <- df_type_3 %>%
    dplyr::filter(
      is_reference_group |
        report_nr %in% report_id_group_level
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_group_id,
      subject_group_id
    ) %>%
    # Base value N total for all variables of a figure
    dplyr::mutate(
      value_n_total_fake_base = sample(0:1000, 1)
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_group_id,
      subject_group_id,
      variable_id
    ) %>%
    # Number of required values for each variable
    dplyr::add_tally(
      name = "value_id_length"
    ) %>%
    # Total N for each variable calculated as a deviation from base value
    dplyr::mutate(
      value_n_total_fake =
        as.integer(
          value_n_total_fake_base *
            stats::runif(1, min = 0.9, max = 1.1)
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(
      report_nr,
      figure_count,
      degree_group_id,
      subject_group_id,
      variable_id,
      value_n_total_fake,
      value_id_length
    )

  df_type_3_fake <- df_type_3 %>%
    dplyr::left_join(
      df_type3_reference_group,
      by = c(
        "report_nr",
        "figure_count",
        "variable_id",
        "degree_group_id",
        "subject_group_id"
      )
    ) %>%
    dplyr::mutate(
      # Total N a random percentage of reference group's total N
      value_n_total_fake = dplyr::if_else(
        is_reference_group |
          report_nr %in% report_id_group_level,
        value_n_total_fake,
        as.integer(
          value_n_total_fake * stats::runif(1, min = 0, max = 1)
        )
      ),
      # Axis label updated with new total N value
      axis_label_fake = stringr::str_replace(
        axis_label,
        pattern = pattern,
        stringr::str_glue(
          "(n={value_n_total_fake})"
        )
      )
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_group_id,
      subject_group_id,
      value_id_length,
      is_reference_group
    ) %>%
    tidyr::nest() %>%
    # Gets vector of fake values for each variable
    dplyr::mutate(
      value_percentage_fake = purrr::map(
        value_id_length,
        get_fake_values_type_3
      )
    ) %>%
    tidyr::unnest(
      cols = data
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      degree_group_id,
      subject_group_id,
      variable_id,
      is_reference_group
    ) %>%
    dplyr::mutate(
      # Helper index for retrieving element from vector of fake values
      value_counter = dplyr::row_number(),
      value_percentage_fake = purrr::map2_dbl(
        value_percentage_fake,
        value_counter,
        ~ .x[.y]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -value_counter,
      -value_id_length
    )

  return(df_type_3_fake)
}

#' Get vector with faked values for figure types 1 and 4
#'
#' @param figure_type_id Integer
#' @param length Integer
#' @param base_value Integer
#'
#' @return Vector with fake values
#' @export
#'
#' @examples
#' get_fake_values_type_1_4(
#'    figure_type_id = 4,
#'    length = 5,
#'    base_value = 800
#'    )
get_fake_values_type_1_4 <- function(figure_type_id, length, base_value = NULL) {
  fake_values <- vector(
    mode = "integer",
    length = length
  )
  factor <- 0
  min <- 0
  max <- 0

  if (is.null(base_value)) {
    base_value <- stats::runif(1, min = 50, max = 1000)
  }

  if (figure_type_id == 1) {
    factor <- stats::runif(1, min = 0, max = 0.25)
    min <- base_value - factor * base_value
    max <- base_value + factor * base_value
  } else if (figure_type_id == 4) {
    factor <- stats::runif(1, min = 0.05, max = 0.40)
    min <- 0.05 * base_value
    max <- factor * base_value
  }

  for (i in seq_along(fake_values)) {
    if (!is.na(base_value)) {
      fake_values[i] <- as.integer(
        stats::runif(1, min = min, max = max)
      )
    }
  }

  return(fake_values)
}

#' Get vector with faked value percentages for figure type 2
#'
#' @param fractions Number of fractions
#' @param periods Number of time periods
#'
#' @return Vector with faked value percentages
#' @export
#'
#' @examples
#' get_fake_values_type_2(
#'    fractions = 6,
#'    periods = 3
#'    )
get_fake_values_type_2 <- function(fractions = 5, periods = 8) {
  df <- dplyr::tibble(
    fraction_id = rep(1:fractions, times = periods),
    period = rep(1:periods, each = fractions),
    percentage = 0
  )

  start_value <- stats::runif(1, min = 0.75, max = 1)
  df[[1, "percentage"]] <- start_value

  for (i in 2:periods) {
    decay_rate <- stats::runif(1, min = 0.75, max = 1)
    lag_value <- df[[((i - 1) * fractions) - fractions + 1, "percentage"]]
    df[[(i * fractions) - fractions + 1, "percentage"]] <-
      lag_value * decay_rate
  }

  df <- create_fake_values_type_2(df)

  return(df[["percentage"]])
}

#' Create faked value percentages for figure type 2
#'
#' @param df Data frame
#'
#' @return Data frame with faked values
#' @export
#'
#' @examples
#' \dontrun{
#' create_fake_values_type_2(df)
#' }
create_fake_values_type_2 <- function(df) {
  residual <- 0
  periods <- length(RUBer::get_unique(df, period))
  fractions <- length(RUBer::get_unique(df, fraction_id))
  length_periods <- periods
  length_fractions <- fractions

  for (i in 1:periods) {
    residual_t0 <- 1 - df[[(i * fractions) - fractions + 1, "percentage"]]
    residual_lag1 <- 0

    if (i > 1) {
      residual_lag1 <- 1 - df[[((i - 1) * fractions) - fractions + 1, "percentage"]]
    }

    # Delta of residual in t0 and lagged by one period is the total amount the
    # other fractions must increase.
    residual <- residual_t0 - residual_lag1

    for (j in 2:fractions)
    {
      if (j == fractions) {
        increase <- residual
      } else {
        increase <- stats::runif(1, min = 0, max = residual)
      }

      lag_value <- 0
      if (i > 1) {
        lag_value <- df[[(i - 1) * length_fractions + j - fractions, "percentage"]]
      }

      residual <- residual - increase
      df[[(i - 1) * length_fractions + j, "percentage"]] <- lag_value + increase
    }
  }
  return(df)
}

#' Get faked value percentages for figure type 3
#'
#' @param x Length
#'
#' @return Numeric vector of length x
#'
#' @examples
#' get_fake_values_type_3(x = 5)
get_fake_values_type_3 <- function(x) {
  fake_values <- vector(
    mode = "double",
    length = x
  )
  residual <- 1

  for (i in seq_along(fake_values)) {
    if (i == length(fake_values)) {
      fake_values[i] <- residual
      residual <- residual - residual
    } else {
      rnd <- stats::runif(1, min = 0, max = residual)
      fake_values[i] <- rnd
      residual <- residual - rnd
    }
  }

  return(fake_values)
}
