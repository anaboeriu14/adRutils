# Summarize NA values by column

Summarize NA values by column

## Usage

``` r
summarize_na(data, threshold = NULL, na_strings = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- threshold:

  Optional numeric in `[0, 100]`. If supplied, only columns at or above
  this missingness percentage are returned.

- na_strings:

  Optional character vector of string values to treat as missing,
  compared after trimming whitespace (so `""` also matches
  whitespace-only entries). Applied to character and factor columns
  only. When `NULL` (default), only true `NA` is counted and the output
  is identical to prior versions.

## Value

A tibble sorted from most to least missing. With `na_strings = NULL`:
columns `column`, `count_na`, `percent_na`. With `na_strings` supplied:
columns `column`, `count_na`, `count_blank`, `count_missing`,
`percent_missing`. Percentages are rounded to 3 decimal places.

## Details

Returns a tibble with one row per column of `data`, listing the count
and percentage of missing values, sorted from most to least missing.

By default only true `NA` values are counted. Supply `na_strings` to
also count empty or sentinel strings (e.g. `""`, `"."`, `"NA"`) as
missing. In that case the output gains `count_blank`, `count_missing`,
and `percent_missing` columns, and `threshold` filters on
`percent_missing`.

## Examples

``` r
df <- data.frame(
  a = c(1, 2, NA, 4, 5),
  b = c("x", "", "  ", "y", NA),
  stringsAsFactors = FALSE
)
summarize_na(df)
#> # A tibble: 2 × 3
#>   column count_na percent_na
#>   <chr>     <dbl>      <dbl>
#> 1 a             1         20
#> 2 b             1         20
summarize_na(df, na_strings = "")
#> # A tibble: 2 × 5
#>   column count_na count_blank count_missing percent_missing
#>   <chr>     <dbl>       <dbl>         <dbl>           <dbl>
#> 1 b             1           2             3              60
#> 2 a             1           0             1              20
```
