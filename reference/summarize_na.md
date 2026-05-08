# Summarize NA values by column

Returns a tibble with one row per column of `data`, listing the count
and percentage of `NA` values, sorted from most to least missing.

## Usage

``` r
summarize_na(data, threshold = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- threshold:

  Optional numeric in `[0, 100]`. If supplied, only columns with
  `percent_na >= threshold` are returned.

## Value

A tibble with columns `column`, `count_na`, and `percent_na` (rounded to
3 decimal places).

## Examples

``` r
df <- data.frame(
  a = c(1, 2, NA, 4, 5),
  b = c(NA, NA, 3, 4, 5),
  c = 1:5
)
summarize_na(df)
#> # A tibble: 3 × 3
#>   column count_na percent_na
#>   <chr>     <dbl>      <dbl>
#> 1 b             2         40
#> 2 a             1         20
#> 3 c             0          0
summarize_na(df, threshold = 50)
#> # A tibble: 0 × 3
#> # ℹ 3 variables: column <chr>, count_na <dbl>, percent_na <dbl>
```
