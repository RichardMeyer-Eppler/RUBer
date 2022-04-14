#' Get system dependent fallback font if a given font is not available
#'
#' @param fonts Data frame obtained by `systemfonts::system_fonts`
#' @param fallback_alias Character, defaults to "sans"
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
#' @return
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
#' @return
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
