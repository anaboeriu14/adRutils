# Replace outliers in numeric variables with `NA`

For each variable in `var_names`, identifies outliers using the IQR rule
(see
[`detect_outlier_thresholds()`](https://anaboeriu14.github.io/adRutils/reference/detect_outlier_thresholds.md))
and replaces them with `NA`. Optionally also nullifies values in paired
columns whenever an outlier is detected in the source column.

## Usage

``` r
replace_outliers_with_na(
  dataf,
  var_names,
  multiplier = 1.5,
  paired_cols = NULL,
  remove_all_na_rows = TRUE,
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame.

- var_names:

  Character vector of numeric column names to check. Non-numeric columns
  in this list are skipped with a warning.

- multiplier:

  IQR multiplier. Default `1.5`.

- paired_cols:

  Optional named list mapping a source column to a paired column. When
  an outlier is detected in the source column, the same row in the
  paired column is also set to `NA`. For example,
  `list(weight = "log_weight")` ensures `log_weight` is set to `NA`
  wherever `weight` was an outlier.

- remove_all_na_rows:

  If `TRUE` (default), drop rows where every variable in `var_names` is
  `NA` after outlier replacement.

- quiet:

  If `TRUE`, suppress messages. Default `FALSE`.

## Value

`dataf` with outliers replaced by `NA` (and possibly fewer rows if
`remove_all_na_rows = TRUE`).

## Details

This function is idempotent: running it twice produces the same result
as running it once, since outliers are replaced with `NA` and `NA`
values are skipped on subsequent passes.

## Examples

``` r
df <- data.frame(
  weight     = c(70, 72, 68, 200, 71),
  log_weight = c(log(70), log(72), log(68), log(200), log(71))
)
replace_outliers_with_na(
  df,
  var_names   = "weight",
  paired_cols = list(weight = "log_weight")
)
#> ℹ Removed 1 all-NA row (20%)
#> ✔ Replaced 1 outlier with NA across 1 variable
#>   weight log_weight
#> 1     70   4.248495
#> 2     72   4.276666
#> 3     68   4.219508
#> 5     71   4.262680
```
