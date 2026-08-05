# Registers RUB Scala TZ font to be used with `showtext` and `systemfonts` packages

Registers RUB Scala TZ font to be used with `showtext` and `systemfonts`
packages

## Usage

``` r
register_font_scala(
  family = get_font_df("RUB Scala TZ.ttf")[["family"]],
  regular = get_font_df("RUB Scala TZ.ttf")[["path"]],
  bold = get_font_df("RUB Scala TZ Bold.ttf")[["path"]],
  italic = get_font_df("RUB Scala TZ Italic.ttf")[["path"]],
  bolditalic = get_font_df("RUB Scala TZ Bold Italic.ttf")[["path"]],
  systemfonts_suffix = "_systemfonts"
)
```

## Arguments

- family:

  Character, font family, defaults to
  `get_font_df("RUB Scala TZ.ttf")[["family"]]`

- regular:

  Character, path of the font file for "regular" font style defaults to
  `get_font_df("RUB Scala TZ.ttf")[["path"]]`

- bold:

  Character, path of the font file for "bold" font style, defaults to
  `get_font_df("RUB Scala TZ Bold.ttf")[["path"]]`

- italic:

  Character, path of the font file for "italic" font style,
  `get_font_df("RUB Scala TZ Italic.ttf")[["path"]]`

- bolditalic:

  Character, path of the font file for "bold italic" font style,
  defaults to `get_font_df("RUB Scala TZ Bold Italic.ttf")[["path"]]`

- systemfonts_suffix:

  Suffix attached to the font family name in
  [`systemfonts::register_font`](https://systemfonts.r-lib.org/reference/register_font.html)

## Value

Side effects

## Examples

``` r
register_font_scala()
#> Warning: ✖ Font file "RUB Scala TZ Bold.ttf" could not be found
#> ℹ Using fallback font "DejaVuSans" instead
#> This warning is displayed once per session.
#> Warning: ✖ Font file "RUB Scala TZ Italic.ttf" could not be found
#> ℹ Using fallback font "DejaVuSans" instead
#> This warning is displayed once per session.
#> Warning: ✖ Font file "RUB Scala TZ Bold Italic.ttf" could not be found
#> ℹ Using fallback font "DejaVuSans" instead
#> This warning is displayed once per session.
```
