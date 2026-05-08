# Select columns by regex pattern

Selects columns from a data frame whose names match any of the supplied
regex patterns, with optional exclusion patterns and an `invert` flag to
return columns that do *not* match.

## Usage

``` r
select_cols_by_pattern(
  dataf,
  patterns,
  exclude = NULL,
  ignore_case = TRUE,
  warn_no_match = TRUE,
  invert = FALSE
)
```

## Arguments

- dataf:

  A data frame or tibble.

- patterns:

  Character vector of regex patterns to match column names.

- exclude:

  Optional character vector of regex patterns. Matching columns are
  dropped after inclusion is applied.

- ignore_case:

  If `TRUE` (default), perform case-insensitive matching.

- warn_no_match:

  If `TRUE` (default), emit a warning and return an empty data frame
  when no columns match. If `FALSE`, abort.

- invert:

  If `TRUE`, return columns that do NOT match the patterns. Default
  `FALSE`.

## Value

A data frame containing the selected columns. When no columns match and
`warn_no_match = TRUE`, a 0-column data frame is returned.

## Details

Multiple patterns are combined with OR logic. Exclusion patterns are
applied after initial matching. When `invert = TRUE`, the function
returns columns that do *not* match any inclusion pattern (after
applying exclusions). Matching is case-insensitive by default.

## Examples

``` r
df <- data.frame(
  id         = 1:5,
  name_first = c("John", "Jane", "Bob", "Alice", "Tom"),
  name_last  = c("Smith", "Doe", "Johnson", "Brown", "Wilson"),
  age        = c(25, 30, 35, 40, 45),
  score_math = c(85, 90, 78, 92, 88),
  score_eng  = c(76, 94, 82, 88, 79)
)

select_cols_by_pattern(df, "name")
#>   name_first name_last
#> 1       John     Smith
#> 2       Jane       Doe
#> 3        Bob   Johnson
#> 4      Alice     Brown
#> 5        Tom    Wilson
select_cols_by_pattern(df, "score", exclude = "math")
#>   score_eng
#> 1        76
#> 2        94
#> 3        82
#> 4        88
#> 5        79
select_cols_by_pattern(df, "score", invert = TRUE)
#>   id name_first name_last age
#> 1  1       John     Smith  25
#> 2  2       Jane       Doe  30
#> 3  3        Bob   Johnson  35
#> 4  4      Alice     Brown  40
#> 5  5        Tom    Wilson  45
select_cols_by_pattern(df, c("id", "age"))
#>   id age
#> 1  1  25
#> 2  2  30
#> 3  3  35
#> 4  4  40
#> 5  5  45
```
