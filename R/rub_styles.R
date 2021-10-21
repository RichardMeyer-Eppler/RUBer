#' Add RUB theme to ggplot chart
#'
#' This function allows you to add the RUB theme to your ggplot charts.
#'
#' @param base_size base font size, defaults to 11
#' @param base_family base font family, defaults to RubFlama
#' @param base_line_size base size for line elements, defaults to base_size/22
#' @param base_rect_size base size for rect elements, defaults to base_size/22
#' @param color Color for font and borders, defaults to `RUB_colors["blue"]`,
#'     i.e. #003560.
#' @param has_facet Boolean indicating whether facet headings are required,
#'     defaults to FALSE.
#' @param x_axis_label Boolean indicating whether there is a label for the
#'     x-axis, defaults to FALSE.
#' @param y_axis_label Boolean indicating whether there is a label for the
#'     y-axis, defaults to FALSE.
#' @param legend_title Boolean indicating whether there is a label for the
#'     legend, defaults to FALSE.
#' @param plot_width Width of the plot in inches, defaults to 6.8
#' @keywords theme_rub
#' @export
#' @examples
#' # Base plot
#' ggplot2::ggplot(
#' mtcars,
#' ggplot2::aes(
#'     x = mpg,
#'     y = disp,
#'     color = as.factor(carb)
#'   )
#' ) +
#' ggplot2::geom_point() +
#' theme_rub()
theme_rub <- function(base_size = 11,
                      base_family = "RubFlama",
                      base_line_size = base_size/22,
                      base_rect_size = base_size/22,
                      color = RUB_colors["blue"],
                      has_facet = FALSE,
                      x_axis_label = FALSE,
                      y_axis_label = FALSE,
                      legend_title = FALSE,
                      plot_width = 6.8) {

  legend_key_size <- plot_width / 100
  legend_key_width <- legend_key_size
  legend_key_height <- legend_key_size * 0.8
  legend_key_spacing <- plot_width / 100

  if(has_facet) {
    facet_text <- ggplot2::element_text(
      size = ggplot2::rel(0.8),
      family = base_family,
      color = color,
      hjust = 0,
      face = "bold"
    )
  } else {
    facet_text <- ggplot2::element_blank()
  }

  if(x_axis_label)  {
    x_axis_text <- ggplot2::element_text(
      hjust = 0.5
    )
  } else  {
    x_axis_text <- ggplot2::element_blank()
  }

  if(y_axis_label)  {
    y_axis_text <- ggplot2::element_text(
      hjust = 0.5
    )
  } else  {
    y_axis_text <- ggplot2::element_blank()
  }

  if(legend_title)  {
    legend_title_text <-  ggplot2::element_text()
  } else  {
    legend_title_text <- ggplot2::element_blank()
  }

  base_theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  rub_theme <- base_theme +
  ggplot2::theme(
    ## Plot
    # Text format
    text = ggplot2::element_text(
      size = base_size,
      color = color
    ),
    # Line format
    line = ggplot2::element_line(
      color = color
    ),
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1),
      face = "bold"
    ),
    # https://stackoverflow.com/questions/25401111/left-adjust-title-in-ggplot2-or-absolute-position-for-ggtitle
    plot.title.position = "plot",
    # This sets the font, size, type and colour of text for the chart's subtitle
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.67),
      face = "italic"
    ),

    ## Plot Border
    plot.background = ggplot2::element_rect(
      color = "black"
    ),

    ## Legend
    legend.position = "bottom",
    legend.title = legend_title_text,
#    legend.text = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
#    legend.key.size =  ggplot2::unit(0.25, "cm"),
    legend.spacing.x = grid::unit(legend_key_spacing, "inch"),
    legend.key.size = grid::unit(legend_key_size, "inch"),
    legend.key.height = grid::unit(legend_key_height, "inch"),
    legend.key.width = grid::unit(legend_key_width, "inch"),

    ## Axis
    # axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.text = ggplot2::element_text(color = color),
    axis.ticks = ggplot2::element_line(),
    axis.line = ggplot2::element_line(),
    axis.title.x = x_axis_text,
    axis.title.y = y_axis_text,

    # Grid lines and panel background
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    # Facet
    strip.background = ggplot2::element_blank(),
    strip.text = facet_text
  )

  return(rub_theme)
}

#' Applies RUB-Style to Flextable object
#'
#' @param table Flextable object
#' @param font Font for the Flextable theme, defaults to RUB Scala TZ
#' @param font_size Font size, defaults to 9
#' @return Styled Flextable object
#' @export
#'
#' @examples
#' table_mtcars <- flextable::flextable(mtcars)
#' rub_style_flextable(table_mtcars)
rub_style_flextable <- function(table,
                                font = "RUB Scala TZ",
                                font_size = 9)  {
  rub_table_font <- font
  header_color <- RUB_colors["green_40"]
  border_green <- RUB_colors["green"]
  border_grey <- "#D7D7D7"

  def_text <- officer::fp_text(
    color = RUB_colors["blue"],
    font.size = font_size,
    font.family = rub_table_font
    )

  def_text_header <- stats::update(
    def_text,
    bold = TRUE
    )

  border = officer::fp_border(
    style = "solid",
    width = 1
    )

  border_v = stats::update(
    border,
    color = border_green
    )

  border_h = stats::update(
    border,
    color = border_grey
    )

  table_formatted <- table %>%
    flextable::style(
      part ="all",
      pr_t = def_text
      ) %>%
    flextable::style(
      part = "header",
      pr_t = def_text_header
      ) %>%
    flextable::bg(
      part = "header",
      bg = header_color
      ) %>%
    flextable::align_text_col(
      align = "left"
      ) %>%
    flextable::border_remove() %>%
    flextable::vline_right(
      part = "body",
      border = border_v
      ) %>%
    flextable::vline_right(
      part = "header",
      border = border_v
      ) %>%
    flextable::border_inner_v(
      part = "body",
      border = border_v
      ) %>%
    flextable::border_inner_v(
      part = "header",
      border = border_v
      ) %>%
    flextable::border_inner_h(
      part = "body",
      border = border_h
      )

  return(table_formatted)
}

#' Format flextable mixed type columns (integer, percentages, NAs)
#'
#' @param x Vector
#'
#' @return Formatted vector
#' @export
#'
#' @examples
#' x <- c("2500", "0.29", NA)
#' rub_format_mixed(x)
rub_format_mixed <- function(x) {
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      x[i] <- ""
    }
    else if (suppressWarnings(
      !is.na(
        as.double(x[i])
        )
      )
    ) {
      if ((as.double(x[i])) %% 1 == 0) {
        x[i] <- as.character(
          format(
            as.integer(x[i]),
            big.mark = ".",
            decimal.mark = ","
          )
        )
      }
      else {
        x[i] <- as.character(
          sprintf(
            "%.01f%%",
            as.double(x[i]) * 100
          )
        )
      }
    }
  }

  return(x)
}

#' Format flextable columns with percentages (percentages, NAs)
#'
#' @param x Vector
#'
#' @return Formatted vector
#' @export
#'
#' @examples
#' x <- c("0.29", NA)
#' rub_format_percent(x)
rub_format_percent <- function(x) {
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      x[i] <- ""
    }

    else if (suppressWarnings(
      !is.na(
        as.double(x[i])
      )
    )) {
      x[i] <- as.character(
        sprintf(
          "%.00f%%",
          as.double(x[i]) * 100
        )
      )
    }
  }

  return(x)
}
