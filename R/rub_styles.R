#' Add RUB theme to ggplot chart
#'
#' This function allows you to add the RUB theme to your ggplot charts.
#'
#' @param font Font for the ggplot theme, defaults to RubFlama
#'
#' @keywords rub_style
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#'   geom_line(colour = "#007f7f", size = 1) +
#'   geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#'   rub_style()
rub_style <- function(font = "RubFlama") {
  font <- font
  rub_blue <- "#003560"

  ggplot2::theme(

    ## Plot
    # Text format
    text = ggplot2::element_text(
      family = font,
      size = 10,
      color = rub_blue
    ),
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      size = 28,
      face = "bold"
    ),
    # This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(
      size = 22
      #      margin = ggplot2::margin(9, 0, 9, 0)
    ),

    # This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    plot.caption = ggplot2::element_text(
      size = 8,
      face = "italic"
    ),

    # Plot Border
    plot.background = ggplot2::element_rect(
      color = "black",
      size = 0.5
    ),

    ## Legend
    legend.position = "bottom",
    legend.key = ggplot2::element_blank(),
    legend.key.size =  ggplot2::unit(0.25, "cm"),

    ## Axis
    axis.text = ggplot2::element_text(
      family = font,
      color = rub_blue
    ),
    #    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_line(color = rub_blue),
    axis.line = ggplot2::element_line(colour = rub_blue),
    axis.title = ggplot2::element_blank(),

    # Grid lines and panel background
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    # Facet
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      family = font,
      color = rub_blue,
      hjust = 0,
      face = "bold"
    )
  )
}

#' Applies RUB-Style to flextable object
#'
#' @param tbl Flextable object
#' @param font Font for the ggplot theme, defaults to RUB Scala TZ
#'
#' @return Styled flextable object
#' @export
#'
#' @examples
#' rub_style_flextable(tbl)
rub_style_flextable <- function(tbl, font = "RUB Scala TZ")  {
  rub_table_font <- font
  rub_blue <- "#003560"
  header_color <- "#D1DF9F"
  border_grey <- "#D7D7D7"
  border_green <- "#8DAE10"

  def_text <- officer::fp_text(color = rub_blue,
                               font.size = 9,
                               font.family = rub_table_font)
  def_text_header <- update(def_text, bold = TRUE)
  border = officer::fp_border(style = "solid", width = 1)
  border_v = update(border, color = border_green)
  border_h = update(border, color = border_grey)

  tbl %>%
    flextable::style(part ="all", pr_t = def_text) %>%
    flextable::style(part = "header", pr_t = def_text_header) %>%
    flextable::bg(part = "header", bg = header_color) %>%
    flextable::align_text_col(align = "left") %>%
    flextable::border_remove() %>%
    flextable::vline_right(part = "body", border = border_v) %>%
    flextable::vline_right(part = "header", border = border_v) %>%
    flextable::border_inner_v(part = "body", border = border_v) %>%
    flextable::border_inner_v(part = "header", border = border_v) %>%
    flextable::border_inner_h(part = "body", border = border_h)
}
