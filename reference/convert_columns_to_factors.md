# Convert columns to factors based on name patterns

Converts columns whose names match any of the supplied regex patterns
into factors. Optionally produces ordered factors.

## Usage

``` r
convert_columns_to_factors(
  dataf,
  patterns,
  exclude = NULL,
  ordered = FALSE,
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame.

- patterns:

  Character vector of regex patterns to match column names.

- exclude:

  Optional character vector of regex patterns. Columns whose names match
  any exclusion pattern are skipped, even if they match an inclusion
  pattern.

- ordered:

  If `TRUE`, create ordered factors. Default `FALSE`.

- quiet:

  If `TRUE`, suppress informational messages. Default `FALSE`.

## Value

`dataf` with matching columns converted to (possibly ordered) factors.

## Examples

``` r
df <- data.frame(
  cdx_var1 = c("A", "B", "A", "C"),
  cdx_var2 = c("X", "Y", "X", "Z"),
  age      = c(25, 30, 22, 40),
  gender   = c("M", "M", "F", "M")
)
convert_columns_to_factors(df, patterns = c("cdx", "gender"))
#> ✔ Converted 3 columns to factors: cdx_var1, cdx_var2, and gender
#>   cdx_var1 cdx_var2 age gender
#> 1        A        X  25      M
#> 2        B        Y  30      M
#> 3        A        X  22      F
#> 4        C        Z  40      M
```
