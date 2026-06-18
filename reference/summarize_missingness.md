# Summarize missing values by column

Returns a tibble with one row per column of `data`, listing the count
and percentage of missing values, sorted from most to least missing.

## Usage

``` r
summarize_missingness(data, threshold = NULL, na_strings = NULL)
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
  only. When `NULL` (default), only true `NA` is counted.

## Value

A tibble sorted from most to least missing. With `na_strings = NULL`:
columns `column`, `count_na`, `percent_na`. With `na_strings` supplied:
columns `column`, `count_na`, `count_blank`, `count_missing`,
`percent_missing`. Percentages are rounded to 3 decimal places.

## Details

By default only `NA` values are counted. Use `na_strings` to also count
empty or sentinel strings (e.g. `""`, `"."`, `"NA"`) as missing. In that
case the output gains `count_blank`, `count_missing`, and
`percent_missing` columns, and `threshold` filters on `percent_missing`.

## See also

[`drop_cols_by_missingness()`](https://anaboeriu14.github.io/adRutils/reference/drop_cols_by_missingness.md)
to remove high-missingness columns.

## Examples

``` r
df <- data.frame(
  a = c(1, 2, NA, 4, 5),
  b = c("x", "", "  ", "y", NA),
  stringsAsFactors = FALSE
)
summarize_missingness(df)
#> # A tibble: 2 × 3
#>   column count_na percent_na
#>   <chr>     <dbl>      <dbl>
#> 1 a             1         20
#> 2 b             1         20
summarize_missingness(df, na_strings = "")
#> # A tibble: 2 × 5
#>   column count_na count_blank count_missing percent_missing
#>   <chr>     <dbl>       <dbl>         <dbl>           <dbl>
#> 1 b             1           2             3              60
#> 2 a             1           0             1              20
```
