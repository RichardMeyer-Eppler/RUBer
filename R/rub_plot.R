#' Plot RUB figure
#'
#' @param df Data Frame bzw. Tibble
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_figure(df)
plot_figure <- function(df) {
  figure_type_id <- unique(df[["figure_type_id"]])

  if (length(figure_type_id) == 1) {
    if (figure_type_id == 1) {
      p <- df %>%
        rub_plot_typ_1(
          x_var = x,
          y_var = y,
          y_label = .[[1, "y_label"]],
          fill_var = fill,
          fill_label = fill_label,
          caption = .[[1, "source_caption"]]
        )
    } else if (figure_type_id == 2) {
      p <- df %>%
        rub_plot_typ_2(
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
        rub_plot_typ_3(
          x = x,
          y = y,
          fill = fill,
          fill_label = fill_label,
          fill_reverse = .[[1, "fill_reverse"]],
          facet = facet,
          group = group,
          caption = .[[1, "source_caption"]]
        )
    }
  } else if (length(figure_type_id) > 1) {
    if (identical(figure_type_id, c(1L, 4L)) |
        identical(figure_type_id, c(4L, 1L))) {
      p <- rub_plot_typ_1_and_4(df)
    }
  }

  return(p)
}

#' Plot vertical stacked bar chart (figure type 1)
#'
#' vertical stacked bar chart in the RUB corporate design. The variables x_var,
#' y_var and fill_var are required, all others are optional.
#'
#' @param df Data frame
#' @param x_var Required variable name for the variable containing the discrete
#'     x-coordinates.
#' @param y_var Required variable name for the variable containing the continuous
#'     y-coordinates.
#' @param y_label Optional label for the y-axis, defaults to an empty string.
#' @param fill_var Variable name for the discrete variable which determines the
#'     groups to be stacked, e.g. degree.
#' @param fill_label Optional variable name for the character variable
#'     containing the names of the fill variable, defaults to NULL.
#' @param caption Optional character containing the data source for the figure
#'     (prefix 'Quelle:' is automatically added).
#' @param filter_cutoff Optional cutoff value for the suppression of data
#'     labels. By default, all values below 0.04 of the total value of the
#'     stacked bar chart are suppressed.
#' @param facet_var Optional variable name for the discrete variable to facet
#'     by, defaults to NULL.
#' @inheritParams rub_style
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Create test values for all three mandatory variables (x_var, y_var, fill_var).
#' df_t1_ex1 <- tibble::tribble(
#' ~term, ~students, ~degree,
#'"Spring '13", 120, "Bachelor 1-Subject",
#' "Spring '14", 105, "Bachelor 1-Subject",
#' "Spring '15", 124, "Bachelor 1-Subject",
#' "Spring '16", 114, "Bachelor 1-Subject",
#'  "Spring '17", 122, "Bachelor 1-Subject",
#'  "Spring '13", 121, "Master 1-Subject",
#'  "Spring '14", 129, "Master 1-Subject",
#'  "Spring '15", 122, "Master 1-Subject",
#'  "Spring '16", 168, "Master 1-Subject",
#'  "Spring '17", 7, "Master 1-Subject",
#' )
#'
#' # The data source is df_t1_ex1, x_var is mapped to term, y_var to students, and
#' # the fill_var to degree.
#' rub_plot_typ_1(
#'  df = df_t1_ex1,
#'  x_var = term,
#'  y_var = students,
#'  fill_var = degree
#' )
rub_plot_typ_1 <- function(df, x_var,
                           y_var, y_label = "",
                           fill_var, fill_label = NULL,
                           caption = "", filter_cutoff = 0.04,
                           facet_var = NULL, color = RUB_colors["blue"],
                           font = "RubFlama") {

  fill_var_sym <- rlang::ensym(fill_var)
  fill_var_unique <- unique(df[[fill_var_sym]])
  fill_var_n_distinct <- dplyr::n_distinct(df[[fill_var_sym]])
  palette <- paste0("discrete_", fill_var_n_distinct)

  fill_label_quo <- rlang::enquo(fill_label)
  fill_label_null <- rlang::quo_is_null(fill_label_quo)

  if(fill_label_null)  {
    fill_label_sym <- rlang::ensym(fill_var)
  } else  {
    fill_label_sym <- rlang::ensym(fill_label)
  }

  fill_label_unique <- unique(df[[fill_label_sym]])

  label_var <- paste0("label_", rlang::quo_name(rlang::enquo(y_var)))
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))

  df_label <- add_label_position_typ_1(
    df,
    x_var = {{x_var}},
    y_var = {{y_var}},
    facet_var = {{facet_var}},
    filter_cutoff = filter_cutoff
  )

  facet_var <- rlang::enquo(facet_var)
  no_facet <- rlang::quo_is_null(facet_var) # See https://rpubs.com/tjmahr/quo_is_missing

  if(no_facet)  {
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

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x_var}},
      y = {{y_var}},
      fill = {{fill_var}}
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        x = {{x_var}},
        y = .data[[label_var]],
        fill = {{fill_var}},
        label = {{y_var}}
      ),
      size = 1.75,
      color = color,
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    facet +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(y_var)
        format(
          {{y_var}},
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
    ) +
    scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      name = NULL,
      label = fill_label_unique
    ) +
    ggplot2::labs(
      y = y_label[1],
      caption = caption
      ) +
    rub_style(
      font = font,
      color = color,
      facet_headings = !no_facet
      ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = 12,
        hjust = 0.5,
        family = font,
        color = color
      )
    )
}

#' Plot horizontal stacked bar charts that are scaled to 100\% (figure type 2)
#'
#' @param fill_reverse Boolean indicating whether the order of the fill variable
#'     should be reversed, default = FALSE.
#' @inheritParams rub_plot_typ_1
#' @inheritParams rub_style
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' rub_plot_typ_2(df = df_fig, x = variable_txt, y = value_percentage, fill = value_id, fill_label = value_txt, fill_reverse = df_fig[[1, "scale_invert_flag"]], facet = degree_id, caption = "Fuck You, Bitch!")
rub_plot_typ_2 <- function(df, x_var,
                           y_var, fill_var,
                           fill_label = NULL, fill_reverse = FALSE,
                           facet_var = NULL, caption = "",
                           filter_cutoff = 0.04, color = RUB_colors["blue"],
                           font = "RubFlama") {

  fill_var_sym <- rlang::ensym(fill_var)
  fill_var_unique <- unique(df[[fill_var_sym]])
  fill_var_n_distinct <- dplyr::n_distinct(df[[fill_var_sym]])
  palette <- paste0("discrete_", fill_var_n_distinct)

  fill_label_quo <- rlang::enquo(fill_label)
  fill_label_null <- rlang::quo_is_null(fill_label_quo)

  if(fill_label_null)  {
    fill_label_sym <- rlang::ensym(fill_var)
  } else  {
    fill_label_sym <- rlang::ensym(fill_label)
  }
  fill_label_unique <- unique(df[[fill_label_sym]])

  # Fill variable needs to be a factor before plotting
  fill_is_factor <- is.factor(df[[fill_var_sym]])

  if(!fill_is_factor) {
    df <- set_factor_var(df, {{fill_var}}, fill_reverse)
  }

  # Fill labels need to be reversed if factor is reversed
  if(fill_reverse)  {
    fill_label_unique <- rev(fill_label_unique)
  }

  label_var <- paste0("label_", rlang::quo_name(rlang::enquo(y_var)))
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))

  df_label <- add_label_position_typ_2(
    df,
    x_var = {{x_var}},
    y_var = {{y_var}},
    facet_var = {{facet_var}},
    filter_cutoff = filter_cutoff,
    reverse = fill_reverse
  )

  facet_var <- rlang::enquo(facet_var)
  no_facet <- rlang::quo_is_null(facet_var) # See https://rpubs.com/tjmahr/quo_is_missing

  if(no_facet)  {
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

  ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df,
      ggplot2::aes(
        x = {{x_var}},
        y = {{y_var}},
        fill = {{fill_var}}
      ),
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        label = sprintf(
          "%1.0f%%",
          {{y_var}} * 100
          ),
        x = {{x_var}},
        y = .data[[label_var]],
        group = {{fill_var}}
      ),
      size = 1.75,
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
      name = NULL,
      label = fill_label_unique
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      reverse = fill_reverse,
      byrow = TRUE
      )
    ) +
    ggplot2::labs(
      caption = caption
      ) +
    rub_style(
      font = font,
      color = color,
      facet_headings = !no_facet
    )
}

#'  Zeichne gestapeltes, rotiertes Balkendiagramm als Anteile von 100 mit Facets (Abbildungstyp 3)
#'
#' @param df Data Frame bzw. Tibble
#' @param x x-Position des Balkens
#' @param y y-Position des Balkens
#' @param fill Gruppierungsvariable für das Stapeln der Säulen (z.B. Abschluss ID)
#' @param fill_label Beschriftung für die einzelnen Werte der Gruppierungsvariablen (z.B. Abschluss DTXT)
#' @param fill_reverse Boolean, ob die Reihenfolge der Gruppierungsvariable invertiert werden soll
#' @param facet Name der Variable, die die Abbildung in Facets aufteilt
#' @param group Weitere Gruppierungsvariable
#' @param caption Quellenangabe für die Abbildung (Präfix 'Quelle:' wird automatisch hinzugefügt)
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (als Bruchteil von 100)
#'
#' @return Ein ggplot Objekt
#' @export
#'
#' @examples
#' rub_plot_typ_3(df = df_fig, x = axis_label, y = value_percentage, fill = value_id, fill_label = value_txt, fill_reverse = df_fig[[1, "scale_invert_flag"]], facet = variable_txt, group = degree_id)
rub_plot_typ_3 <- function(df, x, y,
                           fill, fill_label, fill_reverse,
                           facet, group,
                           caption = "", filter_cutoff = 0.04) {
  label_var <- paste0("label_", rlang::quo_name(rlang::enquo(y)))
  fill_unique <- unique(df[[rlang::ensym(fill)]])
  fill_unique_length <- length(fill_unique)
  palette <- paste0("discrete_", fill_unique_length)
  fill_label <- unique(df[[rlang::ensym(fill_label)]])
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))

  df_label <- RUBer::add_label_position_typ_3(
    df,
    x = {{x}},
    y = {{y}},
    facet = {{facet}},
    group = {{group}},
    fill_reverse = fill_reverse
  ) %>%
    RUBer::filter_label_typ_3(
      y = {{y}},
      filter_cutoff = filter_cutoff
    )

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x}},
      y = {{y}},
      fill = {{fill}},
      order = {{fill}}
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        label = sprintf("%1.0f%%", {{y}} * 100),
        y = .data[[label_var]],
        group = {{fill}}
      ),
      size = 1.75,
      colour = "#003560",
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    ggforce::facet_col(
      ggplot2::vars({{facet}}),
      scales = "free_y",
      space = "free"
    ) +
    RUBer::scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      name = NULL,
      label = fill_label
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      label = scales::percent,
      expand = ggplot2::expand_scale(mult = c(0, .025))) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      reverse = TRUE,
      byrow = TRUE
    )
    ) +
    ggplot2::labs(caption = caption) +
    ggplot2::coord_flip() +
    RUBer::rub_style() +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )
}

#' Zeichne gruppiertes Liniendiagramm (Abbildungstyp 4)
#'
#' @param df Data Frame bzw. Tibble
#' @param x x-Position der Linienpunkte
#' @param y y-Position der Linienpunkte
#' @param group Gruppierungsvariable für die einzelnen Linien (z.B. Abschluss ID)
#' @param group_label Beschriftung der Gruppierungsvariable (z.B. Abschluss DTXT)
#' @param caption Quellenangabe für die Abbildung (Präfix 'Quelle:' wird automatisch hinzugefügt)
#'
#' @return Ein ggplot Objekt
#' @export
#'
#' @examples
#' rub_plot_typ_4(df = df_fig_t4, x = time, y = value_n_total, group = degree_sort, group_label = degree_txt )
rub_plot_typ_4 <- function(df, x, y, group, group_label, caption = "") {
  group_label_unique <- unique(df[[rlang::ensym(group_label)]])
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))
  max_y <- max(df[[rlang::ensym(y)]])

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x}},
      y = {{y}},
      group = {{group}},
      color = {{group}},
      label = {{y}}
    )
  ) +
    ggplot2::geom_line(
      stat = "identity",
      size = 1
    ) +
    ggplot2::geom_label(
      size = 1.75,
      colour = "#003560",
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    RUBer::scale_color_rub(
      palette = "discrete_contrast",
      discrete = TRUE,
      name = NULL,
      labels = group_label_unique
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(y)
        format(
          {{y}},
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        ),
      limits = c(0, max_y) * 1.1
    ) +
    ggplot2::labs(caption = caption) +
    RUBer::rub_style()
}


#' Zeichne gruppiertes Liniendiagramm (Abbildungstyp 5)
#'
#' @param df Data Frame bzw. Tibble
#'
#' @return Ein ggplot Objekt
#' @export
#'
#' @examples
#' rub_plot_typ_1_and_4(df = df)
rub_plot_typ_1_and_4 <- function(df)  {
  plot <- df %>%
    dplyr::filter(figure_type_id == 1) %>%
    RUBer::plot_figure()

  plot_additions <- df %>%
    dplyr::filter(figure_type_id == 4) %>%
    RUBer::add_rub_plot_typ_4(
      x = x,
      y = y,
      group = group,
      group_label = group_label
    )

  plot <- plot +
    rlang::eval_tidy(plot_additions[[1]]) +
    rlang::eval_tidy(plot_additions[[2]]) +
    rlang::eval_tidy(plot_additions[[3]]) +
    rlang::eval_tidy(plot_additions[[4]])

  return(plot)
}

#' Helper Function
#'
#' @param df das
#' @param x das
#' @param y das
#' @param group das
#' @param group_label das
#'
#' @return List of ggplot2 expressions
#' @export
#'
#' @examples
#' add_rub_plot_typ_4(df, x, y, group, group_label)
add_rub_plot_typ_4 <- function(df, x, y, group, group_label) {
  group_label_unique <- unique(df$group_label)
  expr_list <- vector("list", 4)

  expr_list[[1]] <- rlang::quo(
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(
        x = {{x}},
        y = {{y}},
        group = {{group}},
        color = {{group}}
      ),
      stat = "identity",
      size = 1
    )
  )

  expr_list[[2]] <- rlang::quo(
    ggplot2::geom_label(
      data = df,
      ggplot2::aes(
        x = {{x}},
        y = {{y}},
        group = {{group}},
        color = {{group}},
        label = {{y}}
      ),
      size = 1.75,
      colour = "#003560",
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
      name = NULL,
      labels = group_label_unique
    )
  )

  expr_list[[4]] <- rlang::quo(
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2)
    )
  )

  return(expr_list)
}
