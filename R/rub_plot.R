#' Plot RUB figure
#'
#' @param df Data Frame
#' @param font_family Character, the font family to use for all plots, defaults
#'     to `get_font_df()[["family"]]`
#'
#' @return ggplot object
#' @export
#'
#' @example inst/examples/plot_figure.R
plot_figure <- function(
  df,
  font_family = get_font_df()[["family"]]
) {

  figure_type_id <- sort(
    unique(
      df[["figure_type_id"]]
    )
  )

  if (length(figure_type_id) == 1L) {
    if (figure_type_id == 1L) {
      p <- df %>%
        rub_plot_type_1(
          x_var = x,
          x_var_label = x_label,
          y_var = y,
          y_axis_label = .[[1, "y_axis_label"]],
          fill_var = fill,
          fill_label = fill_label,
          caption = .[[1, "source_caption"]],
          base_family = font_family
        )
    } else if (figure_type_id == 2L) {
      p <- df %>%
        rub_plot_type_2(
          x_var = x,
          x_var_label = x_label,
          y_var = y,
          fill_var = fill,
          fill_label = fill_label,
          facet_var = facet,
          caption = .[[1, "source_caption"]],
          filter_cutoff = 0.05,
          base_family = font_family
        )
    } else if (figure_type_id == 3L) {
      p <- df %>%
        rub_plot_type_3(
          x_var = x,
          y_var = y,
          fill_var = fill,
          fill_label = fill_label,
          facet_var = facet,
          title = .[[1, "question_txt"]],
          caption = .[[1, "source_caption"]],
          base_family = font_family
        )
    }
  } else if (length(figure_type_id) > 1) {
    if (identical(figure_type_id, c(1L, 4L))) {
      p <- df %>%
        rub_plot_type_1_and_4(
          x_var = x,
          x_var_label = x_label,
          y_var = y,
          y_axis_label = .[[1, "y_axis_label"]],
          fill_var = fill,
          fill_label = fill_label,
          group_var = group,
          group_label = group_label,
          caption = .[[1, "source_caption"]],
          base_family = font_family
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
#' @keywords internal
#'
#' @examples
#' RUBer:::plot_discrete_palette(12)
plot_discrete_palette <- function(colors_n)  {
  palette <- paste0(
    "discrete_",
    pmin(
      colors_n,
      8L
      )
    )

  if(colors_n > 8)  {
    warning(
      "Number of requested colors for discrete palette exceeds eight.
      No predefined palette for more than eight discrete colors exists
      in RUB_palettes. Additional colors will be interpolated."
    )
  }

  return(palette)
}

#' Plot vertical stacked bar chart (figure type 1)
#'
#' Vertical stacked bar chart in the RUB corporate design. The variables x_var,
#' y_var and fill_var are required, all others are optional.
#'
#' @param df Data frame
#' @param x_var Required variable name for the variable containing the discrete
#'     x-coordinates.
#' @param x_var_label Optional variable name for the character variable
#'     containing the names of the x variable, defaults to NULL.
#' @param y_var Required variable name for the variable containing the
#'     y-coordinates. Will be coerced to numeric with `as.numeric`.
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
#' @family rub_plot_types
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @example inst/examples/rub_plot_type_1.R
rub_plot_type_1 <- function(df,
                           x_var, x_var_label = NULL,
                           y_var, y_axis_label = "",
                           fill_var, fill_reverse = FALSE,
                           fill_label = NULL,
                           caption = "", caption_prefix = "Quelle:",
                           filter_cutoff = 0.04, facet_var = NULL,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = get_font_df()[["family"]], base_size = 11,
                           plot_width = 6.8) {
  # Defuse R expressions
  fill_var_sym <- rlang::ensym(fill_var)
  y_var_quo <-  rlang::enquo(y_var)
  facet_var <- rlang::enquo(facet_var)

  # Booleans
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var)


  # x variable is turned into a factor
  df <- set_factor_var(
    df = df,
    var = {{x_var}},
    var_label = {{x_var_label}}
  )

  # y variable needs to be numeric
  df <- df %>%
    dplyr::mutate(
      !!y_var_quo := as.numeric(
        !!y_var_quo
      )
    )

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
        accuracy = 1L,
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

#' Plot vertical stacked bar charts that are scaled to 100% (figure type 2)
#'
#' @inheritParams rub_plot_type_1
#' @inheritParams theme_rub
#' @param max_width_strip_label Optional maximum width in characters for the facet
#'     label passed to ggplot2::label_wrap_gen.
#' @family rub_plot_types
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @example inst/examples/rub_plot_type_2.R
rub_plot_type_2 <- function(df,
                           x_var, x_var_label = NULL,
                           y_var, y_axis_label = "",
                           fill_var, fill_label = NULL,
                           fill_reverse = FALSE,
                           facet_var = NULL, caption = "",
                           caption_prefix = "Quelle:", filter_cutoff = 0.04,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = get_font_df()[["family"]], base_size = 11,
                           max_width_strip_label = 80) {

  # Defuse R expressions
  fill_var_sym <- rlang::ensym(fill_var)
  y_var_quo <- rlang::enquo(y_var)
  facet_var <- rlang::enquo(facet_var)

  # Booleans
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var)

  # x variable is turned into a factor
  df <- set_factor_var(
    df = df,
    var = {{x_var}},
    var_label = {{x_var_label}}
  )

  # y variable needs to be numeric
  df <- df %>%
    dplyr::mutate(
      !!y_var_quo := as.numeric(
        !!y_var_quo
      )
    )

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
      labeller = ggplot2::label_wrap_gen(
        width = max_width_strip_label
      ),
      ncol = 1,
      scales = "free_y"
    )
  }

  # Plotting function
  ggplot2::ggplot() +
    # Works better than geom_col() for devEMF format
    ggplot2::stat_summary(
      data = df,
      mapping = ggplot2::aes(
        x = {{x_var}},
        y = {{y_var}},
        fill = {{fill_var}}
      ),
      fun = "sum",
      geom = "bar",
      position = "fill",
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

#' Plot horizontal stacked bar charts that are scaled to 100% (figure type 3)
#'
#' @param x_var Required variable name for the variable containing the
#'     x-coordinates. Will be coerced to numeric with `as.numeric`.
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
#' @family rub_plot_types
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @example inst/examples/rub_plot_type_3.R
rub_plot_type_3 <- function(df, x_var,
                           y_var, x_axis_label = NA_character_,
                           fill_var, fill_label = NULL,
                           fill_reverse = FALSE, legend_reverse = FALSE,
                           facet_var = NULL,
                           title = NA_character_, caption = "",
                           caption_prefix = "Quelle:", filter_cutoff = 0.05,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = get_font_df()[["family"]], base_size = 11,
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

  # x variable needs to be numeric
  df <- df %>%
    dplyr::mutate(
      !!x_var_quo := as.numeric(
        !!x_var_quo
      )
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
  # df <- set_factor_var(
  #   df = df,
  #   var = {{fill_var}},
  #   var_label = {{fill_label}},
  #   reverse = !fill_reverse
  # )
  # TO DO: Clean up set_factor_var so it can also respect fixed orderings!
  fill_label_quo <- rlang::enquo(fill_label)
  is_null_fill_label_quo <- rlang::quo_is_null(fill_label_quo)
  if(!is_null_fill_label_quo)  {
    fill_label_sym <- rlang::ensym(fill_label)
    df[[fill_var_sym]] <- forcats::as_factor(df[[fill_label_sym]])
  } else{
    df[[fill_var_sym]] <- forcats::as_factor(df[[fill_var_sym]])
  }
  if(!fill_reverse) {
    df[[fill_var_sym]] <- forcats::fct_rev(df[[fill_var_sym]])
  }

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
        stat = "identity",
        na.rm = TRUE
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
#'     containing the names of the group variable (e.g. degree_txt), defaults to
#'     NULL.
#' @param filter_cutoff Optional integer marking the cutoff below which all
#'     value labels are suppressed, defaults to 5.
#' @inheritParams theme_rub
#' @family rub_plot_types
#'
#' @return A ggplot2 object
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @example inst/examples/rub_plot_type_4.R
rub_plot_type_4 <- function(df,
                           x_var, x_var_label = NULL, x_axis_label = "",
                           y_var, y_axis_label = "",
                           group_var, group_label = NULL,
                           caption = "", caption_prefix = "Quelle:",
                           filter_cutoff = 5, facet_var = NULL,
                           color = RUB_colors["blue"], palette_reverse = FALSE,
                           base_family = get_font_df()[["family"]], base_size = 11) {

  # Defuse R expressions
  y_var_sym <- rlang::ensym(y_var)
  group_var_sym <- rlang::ensym(group_var)
  facet_var_quo <- rlang::enquo(facet_var)
  group_label_quo <- rlang::enquo(group_label)

  # Booleans
  has_x_axis_label <- x_axis_label != ""
  has_y_axis_label <- y_axis_label != ""
  has_facet <- !rlang::quo_is_null(facet_var_quo)
  #has_group_label <- !rlang::quo_is_null(group_label_quo)

  # if(has_group_label) {
  #   group_label_sym <- rlang::ensym(group_label)
  # } else  {
  #   group_label_sym <- rlang::ensym(group_var)
  #   }

  # Plotting variables

  # x variable is turned into a factor
  df <- set_factor_var(
    df = df,
    var = {{x_var}},
    var_label = {{x_var_label}}
  )

  # y variable needs to be numeric
  df <- df %>%
    dplyr::mutate(
      !!y_var_sym := as.numeric(
        !!y_var_sym
      )
    )

  # This function makes sure that the group variable is plotted in the correct
  # order and with the appropriate labels.
  df <- set_factor_var(
    df = df,
    var = {{group_var}},
    var_label = {{group_label}},
    reverse = FALSE
  )

  group_var_levels <- levels(
    df[[group_var_sym]]
  )

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
  colors_n <- dplyr::n_distinct(
    group_var_levels
  )

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
      labels = group_var_levels
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


#' Plot grouped line chart on top of vertical stacked bar chart (combination chart of figure types 1 and 4)
#'
#' @inheritParams rub_plot_type_1
#' @inheritParams rub_plot_type_4
#' @inheritParams theme_rub
#' @family rub_plot_types
#'
#' @return A ggplot object
#' @export
#'
#' @example inst/examples/rub_plot_type_1_and_4.R
rub_plot_type_1_and_4 <- function(df,
                                  x_var, x_var_label = NULL, x_axis_label = "",
                                  y_var, y_axis_label = "",
                                  fill_var, fill_reverse = FALSE,
                                  fill_label = NULL,
                                  group_var, group_label = NULL,
                                  caption = "", caption_prefix = "Quelle:",
                                  filter_cutoff = 0.04, facet_var = NULL,
                                  color = RUB_colors["blue"],
                                  palette_reverse = FALSE,
                                  base_family = get_font_df()[["family"]],
                                  base_size = 11,
                                  plot_width = 6.8)  {

  plot_t1 <- df %>%
    dplyr::filter(
      figure_type_id == 1L
      ) %>%
    rub_plot_type_1(
      x_var = {{x_var}},
      x_var_label = {{x_var_label}},
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

  # Checking if quosures are missing or null
  # https://rpubs.com/tjmahr/quo_is_missing
  fill_label_quo <- rlang::enquo(
    fill_label
  )

  if(
      rlang::quo_is_null(
        fill_label_quo
      )
    ) {
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
      x_var_label = {{x_var_label}},
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
#' @keywords internal
#'
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' add_rub_plot_type_4(df, x, y, group, group_label)
#' }
add_rub_plot_type_4 <- function(df_t4, x_var, x_var_label = NULL,
                               y_var, group_var,
                               group_label = NULL, base_size = 11,
                               base_family = get_font_df()[["family"]],
                               color = RUB_colors["blue"],
                               palette_reverse = FALSE,
                               legend_columns = 5) {

  # Defuse R expressions
  y_var_sym <- rlang::ensym(y_var)
  group_label_quo <- rlang::enquo(group_label)
  group_var_sym <- rlang::ensym(group_var)

  # Booleans
  # has_group_label <- !rlang::quo_is_null(group_label_quo)
  #
  # if(has_group_label) {
  #   group_label_sym <- rlang::ensym(group_label)
  # } else  {
  #   group_label_sym <- rlang::ensym(group_var)
  # }

  # Plotting variables
#  group_label_unique <- unique(df_t4[[group_label_sym]])

  # x variable is turned into a factor
  df_t4 <- set_factor_var(
    df = df_t4,
    var = {{x_var}},
    var_label = {{x_var_label}}
  )

  # y variable needs to be numeric
  df_t4 <- df_t4 %>%
    dplyr::mutate(
      !!y_var_sym := as.numeric(
        !!y_var_sym
      )
    )

  # This function makes sure that the group variable is plotted in the correct
  # order and with the appropriate labels.
  df_t4 <- set_factor_var(
    df = df_t4,
    var = {{group_var}},
    var_label = {{group_label}},
    reverse = FALSE
  )

  group_var_levels <- levels(
    df_t4[[group_var_sym]]
  )

  # Determine required number of discrete colors and get appropriate palette
  colors_n <- dplyr::n_distinct(
    group_var_levels
  )

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
      labels = group_var_levels
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' set_factor_var(df, var, TRUE)
#' }
set_factor_var <- function(df, var, var_label = NULL, reverse = FALSE)  {
  var_sym <- rlang::ensym(var)

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
      df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_label_sym]])
    }
    # var_label effectively replaces var. We only needed var for the ordering.
    # Use of forcats::reorder is not possible, because it only works for numeric
    # and var may not be numeric.
    df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_label_sym]])
  }

  is_factor_var <- is.factor(df[[var_sym]])
  # If var is not yet a factor, it gets gets turned into a factor. Otherwise
  # no action required.
  if(!is_factor_var)  {
    df_ordered[[var_sym]] <- forcats::as_factor(df_ordered[[var_sym]])
  }

  if (reverse) {
    df_ordered[[var_sym]] <- forcats::fct_rev(df_ordered[[var_sym]])
  }

  return(df_ordered)
}

#' Get formula for calculating position of value labels
#'
#' @param label_var The name of the variable requiring value labels
#' @inheritParams add_label_position
#'
#' @return A defused expression for calculating the position of the y-label
#' @keywords internal
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
#' @keywords internal
#'
#' @importFrom rlang :=
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


#' Gets appropriate number of legend columns based on the plot, font and active graphics device
#'
#' @param legend_text Vector with the legend text
#' @param legend_key_width Legend key width
#' @param legend_key_spacing Legend key spacing
#' @param y_axis_text Vector with the text labels of the y axis
#' @inheritParams rub_plot_type_3
#' @inheritParams register_font_df
#' @references
#' * [Unneeded warnings when creating plot using non-default fonts #729](https://github.com/yihui/knitr/issues/729)
#' * [chunk_device in block.R](https://github.com/yihui/knitr/blob/master/R/block.R)
#' * [Access to chunk label #73](https://github.com/yihui/knitr/issues/73)
#'
#' @return Numeric with the number of columns for the legend
#' @export
#'
#' @importFrom grDevices dev.list
#' @importFrom grDevices dev.cur
#' @importFrom grDevices dev.set
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
  base_family = get_font_df()[["family"]],
  systemfonts_suffix = "_systemfonts"
) {

  # If total legend text has fewer than 50 characters, return as many columns
  # as there are elements in the legend text vector
  if(
    sum(
      stringr::str_length(legend_text),
      na.rm = TRUE
    ) < 50
  ) {
    legend_text_length <- length(legend_text)
    return(legend_text_length)
  }

  y_axis_text_max <- y_axis_text[which.max(
    stringr::str_length(y_axis_text)
    )]

  # Debug information
  # rlang::inform(
  #   message = c(
  #     "Device before opening:",
  #     glue::glue(
  #       "Current device is: {.Device}"
  #     ),
  #     glue::glue(
  #       "Available devices are: {.Devices}"
  #     )
  #   )
  # )

  emf_index <- dev.list()["emf"][1]
  dev_cur <- dev.cur()

  if(
    is.null(
      emf_index
    ) ||
    is.na(
      emf_index
    )
  ) {

    file = fs::file_temp(
      ext = "emf"
    )

    fs::dir_create(
      fs::path_temp()
    )

    width = knitr::opts_current$get(
      "fig.width"
    )

    height = knitr::opts_current$get(
      "fig.height"
    )

    font_family_systemfonts <- paste0(
      base_family,
      systemfonts_suffix
    )

    # If no graphics device "emf" in list of devices, open it
    devEMF::emf(
      file = file,
      # file = fs::file_temp(),
      width = width,
      height = height,
      pointsize = base_size,
      family = font_family_systemfonts,
      coordDPI = 72,
      emfPlus = TRUE,
      emfPlusRaster = TRUE,
      emfPlusFontToPath = TRUE
    )
  } else {
    # Switch to emf device in list of devices
    dev.set(
      which = emf_index
    )
  }

  y_axis_text_width <- systemfonts::string_widths_dev(
    strings = y_axis_text_max,
    family = paste0(
      base_family,
      "_systemfonts"
    ),
    face = 1,
    size = base_size,
    cex = 0.9,
    unit = "inches"
  )

  txt_width <- systemfonts::string_widths_dev(
    strings = legend_text,
    family = paste0(
      base_family,
      "_systemfonts"
    ),
    face = 1,
    size = base_size,
    cex = 0.9,
    unit = "inches"
  )

  # Debug information
  # rlang::inform(
  #   message = c(
  #     "Device after opening:",
  #     glue::glue(
  #       "Current device is: {.Device}"
  #     ),
  #     glue::glue(
  #       "Available devices are: {.Devices}"
  #     ),
  #     glue::glue(
  #       "Systemfonts family is: {unique(systemfonts::registry_fonts()[['family']])}"
  #     ),
  #     glue::glue(
  #       "y_axis_text_width is: {y_axis_text_width}"
  #     ),
  #     glue::glue(
  #       "max_txt_width is: {max(txt_width)}"
  #     )
  #   )
  # )

  # Switch back to old device
  dev.set(
    dev_cur
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
