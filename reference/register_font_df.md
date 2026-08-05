# Register font using `sysfonts::font_add` and `systemfonts::register_font`

Registration with
[`sysfonts::font_add`](https://rdrr.io/pkg/sysfonts/man/font_add.html)
exclusively works for `showtext`, while
[`systemfonts::register_font`](https://systemfonts.r-lib.org/reference/register_font.html)
is required for the calculation of string widths, for instance. The
family name must be unique across the two registrations, so the
`systemfonts` registration uses a suffix behind the family name.

## Usage

``` r
register_font_df(
  font_df = RUBer::get_font_df(),
  systemfonts_suffix = "_systemfonts"
)
```

## Arguments

- font_df:

  Data frame with one row obtained by
  [`RUBer::get_font_df()`](get_font_df.md)

- systemfonts_suffix:

  Suffix attached to the font family name in
  [`systemfonts::register_font`](https://systemfonts.r-lib.org/reference/register_font.html)

## Value

Invisibly returns font family

## Examples

``` r
register_font_df()
```
