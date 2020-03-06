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
        RUB::rub_plot_typ_1(
          x = x,
          y = y,
          y_label = .[[1, "y_label"]],
          fill = fill,
          fill_label = fill_label,
          caption = .[[1, "source_caption"]]
        )
    } else if (figure_type_id == 2) {
      p <- df %>%
        RUB::rub_plot_typ_2(
          x = x,
          y = y,
          fill = fill,
          fill_label = fill_label,
          fill_reverse = .[[1, "fill_reverse"]],
          facet = facet,
          caption = .[[1, "source_caption"]]
        )
    } else if (figure_type_id == 3) {
      p <- df %>%
        RUB::rub_plot_typ_3(
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
      p <- RUB::rub_plot_typ_1_and_4(df)
    }
  }

  return(p)
}

#' Zeichne gestapeltes Balkendiagramm (Abbildungstyp 1)
#'
#' Gestapeltes Balkendiagramm im RUB-Corporate Design mit optionalem Label für die y-Achse, optionaler Quellenangabe und der Möglichkeit, Datenlabels zu filtern.
#' @param df Data Frame bzw. Tibble
#' @param x x-Position des Balkens
#' @param y y-Position des Balkens
#' @param y_label Beschriftung der y-Achse
#' @param fill Gruppierungsvariable für das Stapeln der Säulen (z.B. Abschluss ID)
#' @param fill_label Beschriftung für die einzelnen Werte der Gruppierungsvariablen (z.B. Abschluss DTXT)
#' @param caption Quellenangabe für die Abbildung (Präfix 'Quelle:' wird automatisch hinzugefügt)
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (als Bruchteil von 100)
#'
#' @return Ein ggplot Objekt
#' @export
#'
#' @examples
#' rub_plot_typ_1(df = df_fig_t1, x = time, y = value_n_total, fill = degree_sort, fill_label = degree_txt )
rub_plot_typ_1 <- function(df, x, y, y_label = "",
                           fill, fill_label,
                           caption = "", filter_cutoff) {

  fill_label <- rlang::ensym(fill_label)
  fill_label_unique <- unique(df[[fill_label]])
  fill_label_unique_length <- length(fill_label_unique)
  palette <- paste0("discrete_", fill_label_unique_length)
  label_var <- paste0("label_", rlang::quo_name(rlang::enquo(y)))
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))

  df_label <- RUB::add_label_position_typ_1(
    df, x = {{x}}, y = {{y}}
  ) %>%
    RUB::filter_label(
      x = {{x}}, y = {{y}}
    )

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = {{x}},
      y = {{y}},
      fill = {{fill}}
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        x = {{x}},
        y = .data[[label_var]],
        fill = {{fill}},
        label = {{y}}
      ),
      size = 1.75,
      colour = "#003560",
      fill = "white",
      show.legend = FALSE,
      label.r = ggplot2::unit(0, "lines"),
      label.padding = ggplot2::unit(0.10, "lines")
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(y)
        format(
          {{y}},
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
    ) +
    RUB::scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      name = NULL,
      label = fill_label_unique
    ) +
    ggplot2::labs(y = y_label[1], caption = caption) +
    RUB::rub_style() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = 12,
        hjust = 0.5,
        family = "RubFlama",
        color = "#003560"
      )
    )
}

#' Zeichne gestapeltes Balkendiagramm als Anteile von 100 mit Facets (Abbildungstyp 2)
#'
#' @param df Data Frame bzw. Tibble
#' @param x x-Position des Balkens
#' @param y y-Position des Balkens
#' @param fill Gruppierungsvariable für das Stapeln der Säulen (z.B. Abschluss ID)
#' @param fill_label Beschriftung für die einzelnen Werte der Gruppierungsvariablen (z.B. Abschluss DTXT)
#' @param fill_reverse Boolean, ob die Reihenfolge der Gruppierungsvariable invertiert werden soll
#' @param facet Name der Variable, die die Abbildung in Facets aufteilt
#' @param caption Quellenangabe für die Abbildung (Präfix 'Quelle:' wird automatisch hinzugefügt)
#' @param filter_cutoff Schwellwert, ab dem Datenlabels unterdrückt werden (als Bruchteil von 100)
#'
#' @return Ein ggplot Objekt
#' @export
#'
#' @examples
#' rub_plot_typ_2(df = df_fig, x = variable_txt, y = value_percentage, fill = value_id, fill_label = value_txt, fill_reverse = df_fig[[1, "scale_invert_flag"]], facet = degree_id, caption = "Fuck You, Bitch!")
rub_plot_typ_2 <- function(df, x, y,
                           fill, fill_label, fill_reverse = FALSE, facet,
                           caption = "", filter_cutoff) {

  fill_unique <- unique(df[[rlang::ensym(fill)]])
  fill_unique_length <- length(fill_unique)
  palette <- paste0("discrete_", fill_unique_length)
  fill_label <- unique(df[[rlang::ensym(fill_label)]])
  label_var <- paste0("label_", rlang::quo_name(rlang::enquo(y)))
  caption <- ifelse(caption[1] == "", "", paste("Quelle:", caption[1]))

  df_label <- RUB::add_label_position_typ_2(
    df, x = {{x}}, y = {{y}}, facet = {{facet}}
  ) %>%
    RUB::filter_label_typ_2(
      x = {{x}}, y = {{y}}, facet = {{facet}}
    )

  ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df,
      ggplot2::aes(
        x = {{x}},
        y = {{y}},
        fill = {{fill}}
      ),
      stat = "identity",
      width = 0.55
    ) +
    ggplot2::geom_label(
      data = df_label,
      ggplot2::aes(
        label = sprintf("%1.0f%%", {{y}} * 100),
        x = {{x}},
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
    ggplot2::facet_wrap(ggplot2::vars({{facet}}), nrow = 2) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      label = scales::percent
    ) +
    RUB::scale_fill_rub(
      palette = palette,
      discrete = TRUE,
      name = NULL,
      label = fill_label
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      reverse = fill_reverse,
      byrow = TRUE)
    ) +
    ggplot2::labs(caption = caption) +
    RUB::rub_style()
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

  df_label <- RUB::add_label_position_typ_3(
    df,
    x = {{x}},
    y = {{y}},
    facet = {{facet}},
    group = {{group}},
    fill_reverse = fill_reverse
  ) %>%
    RUB::filter_label_typ_3(
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
    # ggplot2::facet_wrap(ggplot2::vars({{facet}}),
    #                     ncol = 1,
    #                     scales = "free_y"
    # ) +
    # ggplot2::facet_grid(
    #   rows = ggplot2::vars({{facet}}),
    #   scales = "free_y",
    #   space = "free_y",
    #   switch = "x"
    # ) +
    ggforce::facet_col(
      ggplot2::vars({{facet}}),
      scales = "free_y",
      space = "free"
    ) +
    RUB::scale_fill_rub(
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
    RUB::rub_style() +
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
    RUB::scale_color_rub(
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
    RUB::rub_style()
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
    RUB::plot_figure()

  plot_additions <- df %>%
    dplyr::filter(figure_type_id == 4) %>%
    RUB::add_rub_plot_typ_4(
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
    RUB::scale_color_rub(
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
