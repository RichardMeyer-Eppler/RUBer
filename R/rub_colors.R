#' RUB colors
#'
#' These are available RUB colors as defined by the corporate design.
#'
#' @references https://www.ruhr-uni-bochum.de/cd/
#' @export
#'
#' @examples
#' RUB_colors["green"]
RUB_colors <- c(
  `green`         = "#8DAE10",
  `green_80`      = "#A4BE40",
  `green_60`      = "#BBCE70",
  `green_40`      = "#D1DF9F",
  `green_20`      = "#E8EFCF",
  `blue`          = "#003560",
  `blue_80`       = "#335D80",
  `blue_60`       = "#6686A0",
  `blue_40`       = "#99AEBF",
  `blue_20`       = "#CCD7DF",
  `lighter grey`  = "#ECECEC",
  `light grey`    = "#E7E7E7",
  `red`           = "#E6332A",
  `dark red`      = "#B61E3E",
  `orange`        = "#ED7102",
  `gold`          = "#FFCC00",
  `brown`         = "#9C5516",
  `tan`           = "#C1BAA3",
  `dark brown`    = "#59201B",
  `asparagus`     = "#8C8751"
)

#' Function to extract RUB palette colors as hex codes
#'
#' @param ... Character names of RUB palette colors
#'
#' @return Vector with specified color hex codes
#' @export
#'
#' @examples
#' get_RUB_colors("green", "blue")
#' get_RUB_colors(1, 3, 1)
get_RUB_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (RUB_colors)

  RUB_colors[cols]
}

#' RUB color palette
#'
#' This is a color palette using the colors defined by the corporate design.
#'
#' @references https://www.ruhr-uni-bochum.de/cd/
#' @export
#'
#' @examples
#' RUB_palettes["discrete_5"]
RUB_palettes <- list(
  `discrete` = get_RUB_colors(
    "green",
    "blue",
    "red",
    "dark red",
    "orange",
    "gold",
    "brown"
  ),
  `discrete_contrast` = get_RUB_colors(
    "gold",
    "red",
    "brown",
    "tan"
  ),
  `discrete_1` = get_RUB_colors(
    "green"
  ),
  `discrete_2` = get_RUB_colors(
    "green",
    "blue"
  ),
  `discrete_3` = get_RUB_colors(
    "green",
    "light grey",
    "blue"
  ),
  `discrete_4` = get_RUB_colors(
    "green",
    "green_60",
    "blue_40",
    "blue"
  ),
  `discrete_5` = get_RUB_colors(
    "green",
    "green_60",
    "light grey",
    "blue_40",
    "blue"
  ),
  `discrete_6` = get_RUB_colors(
    "green",
    "green_80",
    "green_40",
    "blue_40",
    "blue_80",
    "blue"
  ),
  `discrete_7` = get_RUB_colors(
    "green",
    "green_80",
    "green_40",
    "light grey",
    "blue_40",
    "blue_80",
    "blue"
  ),
  `discrete_8` = get_RUB_colors(
    "green",
    "green_80",
    "green_60",
    "green_40",
    "blue_40",
    "blue_60",
    "blue_80",
    "blue"
  ),
  `continuous` = get_RUB_colors(
    "green",
    "blue"
  ),
  `continuous_diverging` = get_RUB_colors(
    "green",
    "light grey",
    "blue"
  )
)

#' Return function to interpolate a RUB color palette
#'
#' @param palette Character name of palette in \code{RUB_palettes}
#' @param reverse Optional boolean indicating whether the palette should be
#'     reversed, defaults to FALSE.
#' @param ... Additional arguments to pass to
#'    \code{\link[grDevices]{colorRamp}}
#'
#' @return Color palette
#' @export
#'
#' @examples
#' get_RUB_palettes(palette = "continuous")
get_RUB_palettes <- function(palette = "discrete", reverse = FALSE, ...) {
  pal <- RUB_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale constructor for RUB colors
#'
#' @inheritParams get_RUB_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or
#'     not, defaults to TRUE.
#' @param ... Additional arguments passed to
#'    \code{\link[ggplot2]{discrete_scale}} or
#'    \code{\link[ggplot2]{scale_gradient}}, used when discrete is TRUE
#'    or FALSE, respectively
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(
#'   data = mtcars,
#'   ggplot2::aes(
#'     x = mpg,
#'     y = disp,
#'     color = as.factor(carb)
#'     )
#'   ) +
#'   ggplot2::geom_point() +
#'   scale_color_rub(
#'     palette = "discrete"
#'   )
scale_color_rub <- function(palette = "discrete",
                            discrete = TRUE,
                            reverse = FALSE,
                            ...) {
  pal <- get_RUB_palettes(
    palette = palette,
    reverse = reverse
  )
  scale_name <- paste0("RUB_", palette)

  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      scale_name = scale_name,
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        label = FALSE,
        ticks = FALSE,
        barwidth = ggplot2::unit(4, "cm"),
        barheight = ggplot2::unit(0.5, "cm"),
        title.vjust = 1
      ),
      ...
    )
  }
}

#' Fill scale constructor for RUB colors
#'
#' @param palette Character name of palette in RUB_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to
#'    \code{\link[ggplot2]{discrete_scale}} or
#'    \code{\link[ggplot2]{scale_gradient}}, used when discrete is TRUE
#'    or FALSE, respectively
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(
#'   data = mtcars,
#'   ggplot2::aes(
#'     x = as.factor(gear),
#'     fill = as.factor(vs)
#'     )
#'   ) +
#'   ggplot2::geom_bar() +
#'   scale_fill_rub(
#'     palette = "discrete_2"
#'   )
scale_fill_rub <- function(palette = "discrete", discrete = TRUE, reverse = FALSE, ...) {
  pal <- get_RUB_palettes(
    palette = palette,
    reverse = reverse
  )
  scale_name <- paste0("RUB_", palette)

  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      scale_name = scale_name,
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        label = FALSE,
        ticks = FALSE,
        barwidth = ggplot2::unit(4, "cm"),
        barheight = ggplot2::unit(0.5, "cm"),
        title.vjust = 1
      ),
      ...
    )
  }
}
