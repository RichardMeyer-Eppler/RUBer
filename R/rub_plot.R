#' Plot RUB figure
#'
#' @param df Data Frame bzw. Tibble
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_figure(df)
#' }
plot_figure <- function(df) {
  figure_type_id <- unique(df[["figure_type_id"]])

  if (length(figure_type_id) == 1) {
    if (figure_type_id == 1) {
      p <- df %>%
        rub_plot_type_1(
          x_var = x,
          y_var = y,
          y_axis_label = .[[1, "y_label"]],
          fill_var = fill,
          fill_label = fill_label,
          caption = .[[1, "source_caption"]]
        )
    } else if (figure_type_id == 2) {
      p <- df %>%
        rub_plot_type_2(
          x_var = x,
          y_var = y,
          fill_var = fill,
          fill_label = fill_label,
          fill_reverse = .[[1, "fill_reverse"]],
          facet_var = facet,
          caption = .[[1, "source_caption"]]
        )
    } else if (figure_type_id == 3) {
      p <- df %>%
        rub_plot_type_3(
          x_var = x,
          y_var = y,
          fill_var = fill,
          fill_label = fill_label,
          fill_reverse = .[[1, "fill_reverse"]],
          facet_var = facet,
#          group = group,
          title = .[[1, "question_txt"]],
          caption = .[[1, "source_caption"]]
        )
    }
  } else if (length(figure_type_id) > 1) {
    if (identical(figure_type_id, c(1L, 4L)) |
        identical(figure_type_id, c(4L, 1L))) {
       p <- df %>%
        rub_plot_type_1_and_4(
          x_var = x,
          y_var = y,
          y_axis_label = .[[1, "y_label"]],
          fill_var = fill,
          fill_label = fill_label,
          fill_reverse = .[[1, "fill_reverse"]],
          group_var = group,
          group_label = group_label,
          caption = .[[1, "source_caption"]]
        )
    }
  }

  return(p)
}

#' Get discrete palette for the plot
#'
#' At the moment, RUB_palettes has discrete palettes for up to eight unique
#'     colors. Above that number, colors are interpolated.
#'
#' @param colors_n Integer for the number of requestes colors in the discrete
#'     palette.
#'
#' @return Name of the appropriate discrete palette from RUB_palettes
#'
#' @examples
#' plot_discrete_palette(12)
plot_discrete_palette <- function(colors_n)  {
  palette <- paste0(
    "discrete_",
    pmin(
      colors_n,
      8L
      )
    )

  if(colors_n > 8)  {
    warning("Number of requested colors for discrete palette exceeds eight.
            No predefined palette for more than eight discrete colors exists
            in RUB_palettes. Additional colors will be interpolated.")
  }

  return(palette)
}

#' Plot vertical stacked bar chart (figure type 1)
#'
#' vertical stacked bar chart in the RUB corporate design. The variables x_var,
#' y_var and fill_var are required, all others are optional.
#'
#' @param df Data frame
#' @param x_var Required variable name for the variable containing the discrete
#'     x-coordinates.
#' @param y_var Required variable name for the variable containing the
#'     continuous y-coordinates.
#' @param y_axis_label Optional label for the y-axis, defaults to an empty
#'     string.
#' @param fill_var Variable name for the discrete variable which determines the
#'     groups to be stacked, e.g. degree.
#' @param fill_reverse Boolean indicating whether the order of the fill variable
#'     should be reversed, defaults to FALSE.
#' @param fill_label Optional variable name for the character variable
#'     containing the names of the fill variable, defaults to NULL.
#' @param caption Optional character containing the data source for the figure
#'     (prefix 'Quelle:' is automatically added).
#' @param caption_prefix Optional character containing the prefix for the
#'     caption, defaults to 'Quelle:'.
#' @param filter_cutoff Optional cutoff value for the suppression of data
#'     labels. By default, all values below 0.04 of the total value of the
#'     stacked bar chart are suppressed.
#' @param facet_var Optional variable name for the discrete variable to facet
#'     by, defaults to NULL.
#' @param palette_reverse Optional boolean indicating whether the colors in the
#'     palette should be reversed, defaults to FALSE.
#' @inheritParams theme_rub
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # Create test values for all three mandatory variables (x_var, y_var,
#' # fill_var).
#' df_t1_ex1 <- tibble::tribble(
#'    ~term, ~students, ~degree,
#'    "Spring '13", 120, "Bachelor 1-Subject",
#'    "Spring '14", 105, "Bachelor 1-Subject",
#'    "Spring '15", 124, "Bachelor 1-Subject",
#'    "Spring '16", 114, "Bachelor 1-Subject",
#'    "Spring '17", 122, "Bachelor 1-Subject",
#'    "Spring '13", 121, "Master 1-Subject",
#'    "Spring '14", 129, "Master 1-Subject",
#'    "Spring '15", 122, "Master 1-Subject",
#'    "Spring '16", 168, "Master 1-Subject",
#'    "Spring '17", 7, "Master 1-Subject",
#' )
#'
#' # The data source is df_t1_ex1, x_var is mapped to term, y_var to students,
#' # and the fill_var to degree.
#' rub_plot_type_1(
#'    df = df_t1_ex1,
#'    x_var = term,
#'    y_var = students,
#'    fill_var = degree
#' )
rub_plot_type_1 <- function(df, x_var,
                           y_var, y_axis_label = "",
                           fill_var, fill_reverse = FALSE,
                           fill_label = NULL,
                           caption = "", caption_prefix = "Quelle:",
                           filter_cutoff = 0.04, facet_var = NULL,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = "RubFlama", base_size = 11,
                           plot_width = 6.8) {
  # Defuse R expressions
  fill_var_sym <- rlang::ensym(fill_var)
  y_var_quo <-  rlang::enquo(y_var)
  facet_var <- rlang::enquo(facet_var)

  # Booleans
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var)

  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(df[[fill_var_sym]])
  palette <- plot_discrete_palette(
    colors_n = colors_n
    )

  # This function makes sure that the fill variable is plotted in the correct
  # order and with the appropriate labels.
  df <- set_factor_var(
    df = df,
    var = {{fill_var}},
    var_label = {{fill_label}},
    reverse = fill_reverse
    )

  # Determines the number of columns to be used for the fill labels in the
  # legend.
  legend_columns <- get_legend_columns(
    legend_text = levels(
      df[[fill_var_sym]]
    ),
    y_axis_text = "",
    plot_width = plot_width,
    base_size = base_size,
    base_family = base_family
  )

  caption <- ifelse(
    caption[1] == "",
    "",
    paste(
      caption_prefix,
      caption[1]
      )
    )
  # Get data frame containing position and values for (filtered) value labels.
  # The y-position of the data labels is contained in the label_var.
  df_label <- add_label_position(
    df,
    x_var = {{x_var}},
    y_var = {{y_var}},
    fill_var = {{fill_var}},
    facet_var = {{facet_var}},
    filter_cutoff = filter_cutoff,
    is_percentage = FALSE
  )

  label_var <- paste0(
    "label_",
    rlang::as_label(
      y_var_quo
    )
  )

  # Set facet element
  if(!has_facet)  {
    facet <- NULL
  } else {
    facet <- ggplot2::facet_wrap(
      ggplot2::vars(
          !!facet_var
        ),
      ncol = 1,
      scales = "free_y"
      )
  }

  # Plotting function
  ggplot2::ggplot(
      mapping = ggplot2::aes(
      x = {{x_var}},
      y = {{y_var}},
      fill = {{fill_var}}
    )
  ) +
    ggplot2::geom_bar(
      data = df,
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        x = {{x_var}},
        y = .data[[label_var]],
        fill = {{fill_var}},
        label = format(
          {{y_var}},
          big.mark = ".",
          decimal.mark = ",",
          trim = TRUE
        )
      ),
      size = base_size / 4,
      family = base_family,
      color = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    facet +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = scales::label_number(
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      reverse = palette_reverse,
      name = NULL
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = FALSE,
        byrow = TRUE,
        ncol = legend_columns
      )
    ) +
    ggplot2::labs(
      y = y_axis_label[1],
      caption = caption
      ) +
    theme_rub(
      base_family = base_family,
      base_size = base_size,
      color = color,
      has_facet = has_facet,
      y_axis_label = has_y_axis_label
      )
}

#' Plot vertical stacked bar charts that are scaled to 100\% (figure type 2)
#'
#' @inheritParams rub_plot_type_1
#' @inheritParams theme_rub
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # Create test values for all three mandatory variables (x_var, y_var, fill_var)
#' df_t2_ex1 <- tibble::tribble(
#'   ~cohort_term, ~status_percentage, ~cohort_status,
#'   "2. cohort term", 0.9513551740, "Studying",
#'   "2. cohort term", 0.0029748098, "Changed subject",
#'   "2. cohort term", 0.0004673679, "Graduated",
#'   "2. cohort term", 0.0186648938, "Disenrolled without degree",
#'   "2. cohort term", 0.0265377545, "Dropped subject",
#'   "4. cohort term", 0.8896149868, "Studying",
#'   "4. cohort term", 0.0616919929, "Changed subject",
#'   "4. cohort term", 0.0016484686, "Graduated",
#'   "4. cohort term", 0.0201024499, "Disenrolled without degree",
#'   "4. cohort term", 0.0269421019, "Dropped subject",
#'   "6. cohort term", 0.7901183540, "Studying",
#'   "6. cohort term", 0.1502641318, "Changed subject",
#'   "6. cohort term", 0.0074548056, "Graduated",
#'   "6. cohort term", 0.0243490259, "Disenrolled without degree",
#'   "6. cohort term", 0.0278136827, "Dropped subject",
#'   "8. cohort term", 0.6115873010, "Studying",
#'   "8. cohort term", 0.2961468339, "Changed subject",
#'   "8. cohort term", 0.0104080044, "Graduated",
#'   "8. cohort term", 0.0274549015, "Disenrolled without degree",
#'   "8. cohort term", 0.0544029593, "Dropped subject",
#' )
#'
#' rub_plot_type_2(
#'   df = df_t2_ex1,
#'   x_var = cohort_term,
#'   y_var = status_percentage,
#'   fill_var = cohort_status
#' )
rub_plot_type_2 <- function(df, x_var,
                           y_var, y_axis_label = "",
                           fill_var, fill_label = NULL,
                           fill_reverse = FALSE,
                           facet_var = NULL, caption = "",
                           caption_prefix = "Quelle:", filter_cutoff = 0.04,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = "RubFlama", base_size = 11) {

  # Defuse R expressions
  fill_var_sym <- rlang::ensym(fill_var)
  y_var_quo <- rlang::enquo(y_var)
  facet_var <- rlang::enquo(facet_var)

  # Booleans
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var)

  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(df[[fill_var_sym]])
  palette <- plot_discrete_palette(
    colors_n = colors_n
  )

  # Make sure that the fill variable is plotted in the correct order and with
  # the appropriate labels.
  df <- set_factor_var(
    df = df,
    var = {{fill_var}},
    var_label = {{fill_label}},
    reverse = fill_reverse
  )

  caption <- ifelse(
    caption[1] == "",
    "",
    paste(
      caption_prefix,
      caption[1]
    )
  )

  # Get data frame containing position and values for (filtered) value labels.
  # The y-position of the data labels is contained in the label_var.
  df_label <- add_label_position(
    df,
    x_var = {{x_var}},
    y_var = {{y_var}},
    fill_var = {{fill_var}},
    facet_var = {{facet_var}},
    filter_cutoff = filter_cutoff,
    is_percentage = TRUE
  )

  label_var <- paste0(
    "label_",
    rlang::as_label(
      y_var_quo
    )
  )

  # Set facet element
  if(!has_facet)  {
    facet <- NULL
  } else {
    facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!facet_var
      ),
      ncol = 1,
      scales = "free_y"
    )
  }

  # Plotting function
  ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df,
      ggplot2::aes(
        x = {{x_var}},
        y = {{y_var}},
        fill = {{fill_var}}
      ),
      position = "fill",
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        x = {{x_var}},
        y = .data[[label_var]],
        group = {{fill_var}},
        label = format(
          {{y_var}},
          big.mark = ".",
          decimal.mark = ",",
          trim = TRUE
        )
      ),
      size = base_size / 4,
      family = base_family,
      color = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    facet +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      label = scales::percent
    ) +
    scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      reverse = palette_reverse,
      name = NULL,
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = FALSE,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(
      y = y_axis_label[1],
      caption = caption
    ) +
    theme_rub(
      base_family = base_family,
      base_size = base_size,
      color = color,
      has_facet = has_facet,
      y_axis_label = has_y_axis_label
    )
}

#' Plot horizontal stacked bar charts that are scaled to 100\% (figure type 3)
#'
#' @param x_axis_label Optional label for the x-axis, defaults to an empty
#'     string.
#' @param legend_reverse Optional boolean indicating whether the legend should
#'     be reverted, defaults to FALSE.
#' @param title Optional plot title
#' @param max_width_axis_text_y Optional maximum width in characters for the
#'     text of the y axis.
#' @param max_width_strip_label Optional maximum width in characters for the facet
#'     label passed to ggplot2::label_wrap_gen.
#' @inheritParams rub_plot_type_1
#' @inheritParams theme_rub
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#' @examples
#' # Create test values for all three mandatory variables (x_var, y_var,
#' # fill_var)
#' df_t3_ex1 <- tibble::tribble(
#'    ~survey_group, ~item_value, ~item_value_percentage,
#'    "Bachelor 1-Subject (n=400)", "Exceeded prescribed period of study", 0.30,
#'    "Bachelor 1-Subject (n=400)", "Within prescribed period of study", 0.70,
#'    "SG Bachelor 1-Subject (n=669)", "Exceeded prescribed period of study", 0.11,
#'    "SG Bachelor 1-Subject (n=669)", "Within prescribed period of study", 0.89
#' )
#'
#' rub_plot_type_3(
#'    df = df_t3_ex1,
#'    x_var = item_value_percentage,
#'    y_var = survey_group,
#'    fill_var = item_value,
#' )
rub_plot_type_3 <- function(df, x_var,
                           y_var, x_axis_label = NA_character_,
                           fill_var, fill_label = NULL,
                           fill_reverse = FALSE, legend_reverse = FALSE,
                           facet_var = NULL,
                           title = NA_character_, caption = "",
                           caption_prefix = "Quelle:", filter_cutoff = 0.05,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = "RubFlama", base_size = 11,
                           max_width_axis_text_y = 30,
                           max_width_strip_label = 80, plot_width = 6.8) {

  # Defuse R expressions
  y_var_sym <- rlang::ensym(y_var)
  fill_var_sym <- rlang::ensym(fill_var)
  x_var_quo <- rlang::enquo(x_var)
  facet_var <- rlang::enquo(facet_var)

  # Booleans
  has_x_axis_label <- !is.na(x_axis_label)
  has_facet <- !rlang::quo_is_null(facet_var)

  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(df[[fill_var_sym]])
  palette <- plot_discrete_palette(
    colors_n = colors_n
  )

  # y variable is turned into a factor before plotting to preserve ordering.
  # Order of y variable always needs to be reverted for this plot type.
  is_factor_y <- is.factor(df[[y_var_sym]])
  if(!is_factor_y) {
    df[[y_var_sym]] <- factor(df[[y_var_sym]], levels = unique(df[[y_var_sym]]))
    df[[y_var_sym]] <- forcats::fct_rev(df[[y_var_sym]])
  }

  # Levels of y-factor should not exceed character limit
  levels(df[[y_var_sym]]) <- stringr::str_wrap(
    levels(df[[y_var_sym]]),
    width = max_width_axis_text_y
  )

  # Make sure that the fill variable is plotted in the correct order and with
  # the appropriate labels.
  df <- set_factor_var(
    df = df,
    var = {{fill_var}},
    var_label = {{fill_label}},
    reverse = !fill_reverse
  )

  # Determines the number of columns to be used for the fill labels in the
  # legend.
  legend_columns <- get_legend_columns(
    legend_text = levels(
      df[[fill_var_sym]]
    ),
    y_axis_text = levels(
      df[[y_var_sym]]
    ),
    plot_width = plot_width,
    base_size = base_size,
    base_family = base_family
  )

  caption <- ifelse(
    caption[1] == "",
    "",
    paste(
      caption_prefix,
      caption[1]
      )
    )

  # Set title for the plot
  get_title <- function(title)
  {
    if(is.na(title[1])) {
      title <- NULL
    } else {
      title <- stringr::str_wrap(
        paste0(
          "Frage: ",
          title[1]
        ),
        width = 90,
        exdent = 13
      )
    }

    return(title)
  }

  title <- get_title(title)

  # Get data frame containing position and values for (filtered) value labels.
  # The function assumes a vertical plot orientation. As figure type 3 is
  # horizontal, x_var and y_var need to be flipped.
  # The x-position of the data labels is contained in the label_var.
  df_label <- add_label_position(
    df,
    x_var = {{y_var}},
    y_var = {{x_var}},
    fill_var = {{fill_var}},
    facet_var = {{facet_var}},
    filter_cutoff = filter_cutoff,
    is_percentage = TRUE
  )

  label_var <- paste0(
    "label_",
    rlang::as_label(
      x_var_quo
    )
  )

  # Set facet element
  if(!has_facet)  {
    facet <- NULL
  } else {
    facet <- ggforce::facet_col(
      ggplot2::vars(
        !!facet_var
      ),
      labeller = ggplot2::label_wrap_gen(
        width = max_width_strip_label
      ),
      scales = "free_y",
      space = "free"
    )
  }

  # Plotting function
  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x_var}},
      y = {{y_var}},
      fill = {{fill_var}}
    )
  ) +
    ggplot2::geom_bar(
        position = "fill",
        stat = "identity"
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        x = .data[[label_var]],
        group = {{fill_var}},
        label = {{x_var}}
      ),
      size = base_size / 4,
      family = base_family,
      colour = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    facet +
    scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      reverse = palette_reverse,
      name = NULL
    ) +
    ggplot2::scale_y_discrete(
      expand = c(0, 0)
      ) +
    ggplot2::scale_x_continuous(
      label = scales::percent,
      expand = ggplot2::expansion(
        mult = c(0, .025)
        )
      ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = !legend_reverse,
        byrow = TRUE,
        ncol = legend_columns
      )
    ) +
    ggplot2::labs(
      title = title,
      caption = caption,
      x = x_axis_label[1]
    ) +
    theme_rub(
      base_family = base_family,
      base_size = base_size,
      color = color,
      has_facet = has_facet,
      x_axis_label = has_x_axis_label,
      plot_width = plot_width
    ) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )
}

#' Plot grouped line chart (figure type 4)
#'
#' @inheritParams rub_plot_type_1
#' @inheritParams rub_plot_type_3
#' @param group_var Variable name for the discrete variable which determines the
#'     groups forming one line, e.g. degree_id.
#' @param group_label Optional variable name for the character variable
#'     containing the names of the group variable (e.g. degree_txt), defaults to #'     NULL.
#' @param filter_cutoff Optional integer marking the cutoff below which all
#'     value labels are suppressed, defaults to 5.
#' @inheritParams theme_rub
#'
#' @return Ein ggplot Objekt
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' rub_plot_type_4(
#'    df = df_fig_t4,
#'    x = time,
#'    y = value_n_total,
#'    group = degree_sort,
#'    group_label = degree_txt
#' )
#' }
rub_plot_type_4 <- function(df, x_var, x_axis_label = "",
                           y_var, y_axis_label = "",
                           group_var, group_label = NULL,
                           caption = "", caption_prefix = "Quelle:",
                           filter_cutoff = 5, facet_var = NULL,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = "RubFlama", base_size = 11) {

  # Defuse R expressions
  y_var_sym <- rlang::ensym(y_var)
  group_var_sym <- rlang::ensym(group_var)
  facet_var_quo <- rlang::enquo(facet_var)
  group_label_quo <- rlang::enquo(group_label)

  # Booleans
  has_x_axis_label <- x_axis_label != ""
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var_quo)
  has_group_label <- !rlang::quo_is_null(group_label_quo)

  if(has_group_label) {
    group_label_sym <- rlang::ensym(group_label)
  } else  {
    group_label_sym <- rlang::ensym(group_var)
    }

  # Plotting variables
  group_label_unique <- unique(df[[group_label_sym]])
  max_y <- max(df[[y_var_sym]])
  caption <- ifelse(
    caption[1] == "",
    "",
    paste(
      caption_prefix,
      caption[1]
    )
  )

  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(df[[group_var_sym]])

  # Set facet element
  if(!has_facet)  {
    facet <- NULL
  } else {
    facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!facet_var_quo
      )
 #     ,ncol = 1
    )
  }

  df_label <- dplyr::filter(
    df,
    {{y_var}} >= filter_cutoff
    )

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x_var}},
      y = {{y_var}},
      group = {{group_var}},
      color = {{group_var}},
      label = format(
        {{y_var}},
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
    )
  ) +
    ggplot2::geom_line(
      stat = "identity",
      size = base_size / 10
    ) +
    ggplot2::geom_label(
      data = df_label,
      size = base_size / 4,
      family = base_family,
      colour = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    facet +
    scale_color_rub(
      palette = "discrete",
      discrete = TRUE,
      reverse = palette_reverse,
      name = NULL,
      labels = group_label_unique
      ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(y_var)
        format(
          {{y_var}},
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE,
          trim = TRUE
        ),
      limits = c(0, max_y) * 1.1
    ) +
    ggplot2::labs(
      caption = caption,
      x = x_axis_label[1],
      y = y_axis_label[1]
      ) +
    theme_rub(
      base_family = base_family,
      base_size = base_size,
      color = color,
      has_facet = has_facet,
      x_axis_label = has_x_axis_label,
      y_axis_label = has_y_axis_label
      )
}


#' Plot grouped line chart on top of vertical stacked bar chart (figure type 5)
#'
#' @inheritParams rub_plot_type_1
#' @inheritParams rub_plot_type_4
#' @inheritParams theme_rub
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' rub_plot_type_1_and_4(df = df)
#' }
rub_plot_type_1_and_4 <- function(df, x_var, x_axis_label = "",
                                  y_var, y_axis_label = "",
                                  fill_var, fill_reverse = FALSE,
                                  fill_label = NULL,
                                  group_var, group_label = NULL,
                                  caption = "", caption_prefix = "Quelle:",
                                  filter_cutoff = 0.04, facet_var = NULL,
                                  color = RUB_colors["blue"],
                                  palette_reverse = FALSE,
                                  base_family = "RubFlama",
                                  base_size = 11,
                                  plot_width = 6.8)  {
  plot_t1 <- df %>%
    dplyr::filter(
      figure_type_id == 1L
      ) %>%
    rub_plot_type_1(
      x_var = {{x_var}},
      y_var = {{y_var}},
      y_axis_label = y_axis_label,
      fill_var = {{fill_var}},
      fill_reverse = fill_reverse,
      fill_label ={{fill_label}},
      caption = caption,
      caption_prefix = caption_prefix,
      filter_cutoff = filter_cutoff,
      facet_var = {{facet_var}},
      color = color,
      palette_reverse = palette_reverse,
      base_family = base_family,
      base_size = base_size
      )

  if(is.null(fill_label)) {
    legend_text <- unique(
      df[[rlang::ensym(fill_var)]]
    )
  } else {
    legend_text <- unique(
      df[[rlang::ensym(fill_label)]]
    )
  }

  # Determines the number of columns to be used for the fill labels in the
  # legend.
  legend_columns <- get_legend_columns(
    legend_text = legend_text,
    y_axis_text = "PLATZHALTERTEXT",
    plot_width = plot_width,
    base_size = base_size,
    base_family = base_family
  )

  plot_t4_additions <- df %>%
    dplyr::filter(
      figure_type_id == 4L
      ) %>%
    add_rub_plot_type_4(
      x_var = {{x_var}},
      y_var = {{y_var}},
      group_var = {{group_var}},
      group_label = {{group_label}},
      legend_columns = legend_columns,
      base_size = base_size,
      base_family = base_family,
      color = color,
      palette_reverse = palette_reverse
    )


  plot_t1_t4 <- plot_t1 +
    rlang::eval_tidy(
      plot_t4_additions[[1]]
      ) +
    rlang::eval_tidy(
      plot_t4_additions[[2]]
      ) +
    rlang::eval_tidy(
      plot_t4_additions[[3]]
      ) +
    rlang::eval_tidy(
      plot_t4_additions[[4]]
      )

  return(plot_t1_t4)
}

#' Helper Function
#' @param df_t4 Data frame
#' @inheritParams rub_plot_type_1_and_4
#'
#' @return List of ggplot2 expressions
#'
#' @examples
#' \dontrun{
#' add_rub_plot_type_4(df, x, y, group, group_label)
#' }
add_rub_plot_type_4 <- function(df_t4, x_var,
                               y_var, group_var,
                               group_label = NULL, base_size = 11,
                               base_family = "RubFlama",
                               color = RUB_colors["blue"],
                               palette_reverse = FALSE,
                               legend_columns = 5) {

  # Defuse R expressions
  group_label_quo <- rlang::enquo(group_label)
  group_var_sym <- rlang::ensym(group_var)

  # Booleans
  has_group_label <- !rlang::quo_is_null(group_label_quo)

  if(has_group_label) {
    group_label_sym <- rlang::ensym(group_label)
  } else  {
    group_label_sym <- rlang::ensym(group_var)
  }

  # Plotting variables
  group_label_unique <- unique(df_t4[[group_label_sym]])
  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(df_t4[[group_var_sym]])

  expr_list <- vector("list", 4)

  expr_list[[1]] <- rlang::quo(
    ggplot2::geom_line(
      data = df_t4,
      ggplot2::aes(
        x = {{x_var}},
        y = {{y_var}},
        group = {{group_var}},
        color = {{group_var}}
      ),
      stat = "identity",
      size = base_size / 10,
      inherit.aes = FALSE
    #  ,show.legend = FALSE
    )
  )

  expr_list[[2]] <- rlang::quo(
    ggplot2::geom_label(
      data = df_t4,
      ggplot2::aes(
        x = {{x_var}},
        y = {{y_var}},
        group = {{group_var}},
        color = {{group_var}},
        label = format(
          {{y_var}},
          big.mark = ".",
          decimal.mark = ",",
          trim = TRUE
        )
      ),
      size = base_size / 4,
      family = base_family,
      colour = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    )
  )

  expr_list[[3]] <- rlang::quo(
    RUBer::scale_color_rub(
      palette = "discrete_contrast",
      discrete = TRUE,
      reverse = palette_reverse,
      name = NULL,
      labels = group_label_unique
    )
  )

  expr_list[[4]] <- rlang::quo(
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1,
        reverse = FALSE,
        byrow = TRUE,
        ncol = legend_columns
        ),
      color = ggplot2::guide_legend(
        order = 2
        )
      )
    )

  return(expr_list)
}

#' Prepares var and var_label columns for plotting by ordering them and turning
#' them into factors
#'
#' @param df Data frame
#' @param var Required variable name for the discrete variable to be turned into
#'     a factor. This variable is sorted alphabetically to determine the order.
#' @param var_label Optional variable name for the discrete variable labels
#'     to be used instead of var. If var is a sort key, for instance, var_label
#'     can be used for the actual labels.
#' @param reverse Whether the order of the factor should be reverted, defaults
#'     to FALSE.
#'
#' @return Data frame with factor column.
#'
#' @examples
#' \dontrun{
#' set_factor_var(df, var, TRUE)
#' }
set_factor_var <- function(df, var, var_label = NULL, reverse = FALSE)  {
  var_sym <- rlang::ensym(var)
  # https://community.rstudio.com/t/unquoting-issue-in-purrr-map-df-function/106676/2
  # Required in call to forcats::fct_rev()
  var_name <- rlang::as_name(var_sym)

  # Var is always used to determine the ordering, even if var_label will later
  # get displayed. This allows, for instance, to have sort_keys in var and the
  # associated labels in var_label.
  # Note that unlike base factor, forcats::as_factor does not alter ordering
  df_ordered <- dplyr::arrange(df, {{var}})

  var_label_quo <- rlang::enquo(var_label)
  is_null_var_label_quo <- rlang::quo_is_null(var_label_quo)

  if(!is_null_var_label_quo)  {
    var_label_sym <- rlang::ensym(var_label)

    # If var_label is not a factor yet, it will get converted to one
    is_factor_var_label <- is.factor(df_ordered[[var_label_sym]])
    if(!is_factor_var_label)  {
      df_ordered[[var_name]] <- forcats::as_factor(df_ordered[[var_label_sym]])
    }
    # var_label effectively replaces var. We only needed var for the ordering.
    # Use of forcats::reorder is not possible, because it only works for numeric
    # and var may not be numeric.
    df_ordered[[var_name]] <- forcats::as_factor(df_ordered[[var_label_sym]])
  }

  is_factor_var <- is.factor(df[[var_name]])
  # If var is not yet a factor, it gets gets turned into a factor. Otherwise
  # no action required.
  if(!is_factor_var)  {
    df_ordered[[var_name]] <- forcats::as_factor(df_ordered[[var_name]])
  }

  if (reverse) {
    df_ordered[[var_name]] <- forcats::fct_rev(df_ordered[[var_name]])
  }

  return(df_ordered)
}

#' Get formula for calculating position of value labels
#'
#' @param label_var The name of the variable requiring value labels
#' @inheritParams add_label_position
#'
#' @return A defused expression for calculating the position of the y-label
#' @export
#'
#' @examples
#' \dontrun{
#' get_label_formula(y_var = cyl, label_reverse = TRUE, is_percentage = FALSE)
#' }
get_label_formula <- function(label_var,
                              is_percentage = FALSE) {
  label_var <- rlang::enquo(label_var)

  if(!is_percentage) {
    label_formula <- rlang::expr(
      sum(!!label_var) - cumsum(!!label_var) + !!label_var / 2
    )
  } else if(is_percentage)  {
    label_formula <- rlang::expr(
      (sum(!!label_var) - cumsum(!!label_var) + !!label_var / 2) / sum(!!label_var)
    )
  }

  return(label_formula)
}


#' Add centered y-coordinates for filtered data labels of figure types 1 and 2
#'
#' Calculates the y-coordinates for stacked bar charts, centered for
#'    each group. The default function does not work if some labels are
#'    filtered.
#'
#' @inheritParams rub_plot_type_2
#' @param is_percentage Optional boolean indicating whether the value label is
#'     expressed in absolute numbers or as a percentage, defaults to false
#'
#' @return Data frame with additional column "label_" + the name of the
#'    y-coordinate variable
#'
#' @examples
#' \dontrun{
#' add_label_position(df, x_var, y_var, fill)
#' }
add_label_position <- function(df, x_var,
                               y_var, facet_var = NULL,
                               fill_var, filter_cutoff = 0.05,
                               fill_reverse = FALSE, is_percentage = FALSE) {

  has_facet <- !rlang::quo_is_null(
    rlang::enquo(
      facet_var
    )
  )

  # Group vars with and without fill_var for different summary statistics
  # Caution: Order of grouping vars matters for dplyr::summarise!
  if(!has_facet)  {
    group_vars <- dplyr::vars(
      rlang::ensym(x_var),
      rlang::ensym(fill_var)
      )
    } else if(has_facet) {
    group_vars <- dplyr::vars(
      rlang::ensym(x_var),
      rlang::ensym(facet_var),
      rlang::ensym(fill_var)
    )
  }

  y_var <- rlang::enquo(y_var)

  label_formula <- get_label_formula(
    label_var = {{y_var}},
    is_percentage = is_percentage
  )

  # y formula
  if(is_percentage) {
    y_formula <- rlang::expr(
      !!y_var / sum(!!y_var)
    )
  } else  {
    y_formula <- rlang::expr(
      !!y_var
    )
  }

  df_label <- df %>%
    dplyr::group_by_at(
      group_vars
    ) %>%
    dplyr::summarise(
      !!y_var := sum(!!y_var)
    ) %>%
    dplyr::mutate(
      # sum_var = sum(!!y_var),
      # cumsum_var = cumsum(!!y_var),
      # half_var_var = !!y_var / 2,
      "label_{{y_var}}" := eval(label_formula)
    )

  if(is_percentage) {
      df_label <- df_label %>%
        dplyr::ungroup(
          {{fill_var}}
        ) %>%
        dplyr::mutate(
          !!y_var := eval(y_formula)
        )
  }

  df_label_filtered <- df_label %>%
    dplyr::filter(
      round((!!y_var / sum(!!y_var)), digits = 2) >= filter_cutoff
    )

  # Format percentage values with no decimals and percentage sign
  # https://community.rstudio.com/t/using-label-percent-with-the-label-argument-of-geom-text-and-geom-label/54244/2
  if(is_percentage) {
    df_label_formatted <- df_label_filtered %>%
      dplyr::mutate(
        !!y_var := scales::label_percent(
          accuracy = 1L
        )(!!y_var)
      )
    } else  {
      df_label_formatted <- df_label_filtered
      }

  return(df_label_formatted)
}


#' Gets number of legend columns based on the plot width and the text base size
#'
#' @param legend_text Vector with the legend text
#' @param legend_key_width Legend key width
#' @param legend_key_spacing Legend key spacing
#' @param y_axis_text Vector with the text labels of the y axis
#' @inheritParams rub_plot_type_3
#'
#' @return Numeric with the number of columns for the legend
#' @export
#'
#' @examples
#' get_legend_columns(
#'   legend_text = c(
#'     "1 - eigener Verdienst",
#'     "2 - Mittel der Eltern/Verwandten",
#'     "3 - Förderung nach BAföG",
#'     "4 - Stipendium",
#'     "5 - Sonstiges"
#'   ),
#'   y_axis_text = c(
#'     "Bachelor 2-Fächer (n=251)",
#'     "FG Bachelor 2-Fächer (n=1.310)"
#'   )
#' )
get_legend_columns <- function(
  legend_text,
  y_axis_text,
  legend_key_width = plot_width / 100,
  legend_key_spacing = plot_width / 100,
  plot_width = 6.8,
  base_size = 11,
  base_family = "RubFlama"
) {

  # If total legend text has fewer than 50 characters, return as many columns
  # as there are elements in the legend text vector
  if(
    sum(
      stringr::str_length(legend_text),
      na.rm = TRUE
    ) < 50
  ) {
    test <- length(legend_text)
    return(test)
  }

  y_axis_text_max <- y_axis_text[which.max(
    stringr::str_length(y_axis_text)
    )]

  y_axis_text_width <- strwidth(
    y_axis_text_max,
    units = "inches",
    family = base_family,
    cex = 0.9
  )

  txt_width <- strwidth(
    legend_text,
    units = "inches",
    family = base_family,
    cex = 0.9
  )

  col_width <- max(txt_width) +
    legend_key_width +
    2 * legend_key_spacing

  legend_columns <- (plot_width - y_axis_text_width) %/% col_width

  legend_columns <- max(
    legend_columns,
    1
  )

  return(legend_columns)
}
