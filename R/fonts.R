#' Get system dependent fallback font if a given font is not available
#'
#' This functions returns the system dependent font for the alias "sans".
#'
#' The aliases are mapped the following way:
#' * "" and "sans" return Helvetica on Mac, Arial on Windows, and the default sans-serif font on Linux (DejaVu Sans on Ubuntu)
#' * "serif" return Times on Mac, Times New Roman on Windows, and the default serif font on Linux (DejaVu Serif on Ubuntu)
#' * "mono" return Courier on Mac, Courier New on Windows, and the default mono font on Linux (DejaVu Mono on Ubuntu)
#' * "emoji" return Apple Color Emoji on Mac, Segoe UI Emoji on Windows, and the default emoji font on Linux (Noto Color on Ubuntu)
#'
#' See https://github.com/r-lib/systemfonts for details
#'
#' @param fonts Data frame obtained by `systemfonts::system_fonts`
#' @param fallback_alias Character, one of `c("sans", "serif", "mono", "emoji")`
#'     defaults to "sans".
#'
#' @return Data frame obtained by `systemfonts::system_fonts` with one row
#' @export
#'
#' @examples
#' get_fallback_font_df()
get_fallback_font_df <- function(
  fonts = systemfonts::system_fonts(),
  fallback_alias = "sans"
) {

  fallback_alias <- rlang::arg_match(
    fallback_alias,
    values = c(
      "sans", "serif", "mono", "emoji"
    )
  )

  fallback_font <- systemfonts::match_font(
    fallback_alias
  )

  df_font <- fonts %>%
    dplyr::filter(
      tolower(
        fs::path_file(
          .data[["path"]]
        )
      ) == tolower(
        fs::path_file(
          fallback_font[["path"]]
        )
      )
    )

  return(df_font)
}

#' Get data frame of font information for a font file
#'
#' @param font_file Character, name of the font file (case insensitive)
#'
#' @return Data frame obtained by `systemfonts::system_fonts` with one row
#' @export
#'
#' @examples
#' get_font_df("RUB Scala TZ.ttf")
get_font_df <- function(
  font_file = "RubFlama-Regular.ttf"
) {
  fonts <- systemfonts::system_fonts()

  df_font <- fonts %>%
    dplyr::filter(
      tolower(
        fs::path_file(
          .data[["path"]]
        )
      ) == tolower(font_file)
    )

  if(
    nrow(df_font) == 0L
  ) {

    df_font <- get_fallback_font_df(
      fonts
    )

    font_name <- df_font[["name"]]

    rlang::warn(
      message = c(
        "x" = glue::glue(
          'Font file "{font_file}" could not be found'
        ),
        "i" = glue::glue(
          'Using fallback font "{font_name}" instead'
        )
      ),
      .frequency = "once",
      .frequency_id = font_file
    )
  }

  return(df_font)
}

#' Register font contained in font data frame
#'
#' @param font_df Data frame with one row obtained by `RUBer::get_font_df()`
#'
#' @return Invisibly returns font family
#' @export
#'
#' @examples
#' register_font_df()
register_font_df <- function(
  font_df = RUBer::get_font_df()
) {

  font_family <- font_df[["family"]]
  font_path <- font_df[["path"]]
  font_file <- fs::path_file(
    font_path
  )

  if(
    font_file == "RubFlama-Regular.ttf"
  ) {
    RUBer::register_font_flama()
  } else if(
    font_file == "RUB Scala TZ.ttf"
  ) {
    RUBer::register_font_scala()
  } else {
    sysfonts::font_add(
      family = font_family,
      regular = font_file
    )
  }

  return(
    invisible(
      font_family
    )
  )
}

#' Registers RUB Flama font to be used with `showtext` package
#'
#' @param family Character, font family, defaults to
#'     `get_font_df()[["family"]]`
#' @param regular Character, path of the font file for "regular" font style
#'     defaults to `get_font_df()[["path"]]`
#' @param bold Character, path of the font file for "bold" font style,
#'     defaults to `get_font_df("RubFlama-Bold.ttf")[["path"]]`
#' @param italic Character, path of the font file for "italic" font style,
#'     `get_font_df("RubFlama-Italic.ttf")[["path"]]`
#' @param bolditalic Character, path of the font file for "bold italic" font style,
#'     defaults to `get_font_df("RubFlama-BoldItalic.ttf")[["path"]]`
#'
#' @return Side effects
#' @export
#'
#' @examples
#' register_font_flama()
register_font_flama <- function(
  family = get_font_df()[["family"]],
  regular = get_font_df()[["path"]],
  bold = get_font_df("RubFlama-Bold.ttf")[["path"]],
  italic = get_font_df("RubFlama-Italic.ttf")[["path"]],
  bolditalic = get_font_df("RubFlama-BoldItalic.ttf")[["path"]]
) {

  sysfonts::font_add(
    family = family,
    regular = regular,
    bold = bold,
    italic = italic,
    bolditalic = bolditalic
  )

}

#' Registers RUB Scala TZ font to be used with `showtext` package
#'
#' @param family Character, font family, defaults to
#'     `get_font_df("RUB Scala TZ.ttf")[["family"]]`
#' @param regular Character, path of the font file for "regular" font style
#'     defaults to `get_font_df("RUB Scala TZ.ttf")[["path"]]`
#' @param bold Character, path of the font file for "bold" font style,
#'     defaults to `get_font_df("RUB Scala TZ Bold.ttf")[["path"]]`
#' @param italic Character, path of the font file for "italic" font style,
#'     `get_font_df("RUB Scala TZ Italic.ttf")[["path"]]`
#' @param bolditalic Character, path of the font file for "bold italic" font style,
#'     defaults to `get_font_df("RUB Scala TZ Bold Italic.ttf")[["path"]]`
#'
#' @return Side effects
#' @export
#'
#' @examples
#' register_font_scala()
register_font_scala <- function(
  family = get_font_df("RUB Scala TZ.ttf")[["family"]],
  regular = get_font_df("RUB Scala TZ.ttf")[["path"]],
  bold = get_font_df("RUB Scala TZ Bold.ttf")[["path"]],
  italic = get_font_df("RUB Scala TZ Italic.ttf")[["path"]],
  bolditalic = get_font_df("RUB Scala TZ Bold Italic.ttf")[["path"]]
) {

  sysfonts::font_add(
    family = family,
    regular = regular,
    bold = bold,
    italic = italic,
    bolditalic = bolditalic
  )

}
