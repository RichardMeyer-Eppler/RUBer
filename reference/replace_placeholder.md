# Replace placeholder strings in docx document

Replace placeholder strings in docx document

## Usage

``` r
replace_placeholder(
  doc,
  placeholder_text = "PLACEHOLDER_TAB",
  replacement_text = "\t"
)
```

## Arguments

- doc:

  an rdocx object

- placeholder_text:

  Character with the placeholder to be replaced, defaults to
  "PLACEHOLDER_TAB"

- replacement_text:

  Character with replacement text, defaults to "\t"

## Value

Side effects

## Examples

``` r
if (FALSE) { # \dontrun{
replace_placeholder(
   path = "test.docx",
   new_path = "test_replaced.docx"
)
} # }
```
