# Detect outlier thresholds via the IQR rule

Computes lower and upper outlier bounds for a numeric variable using the
IQR rule: `Q1 - k*IQR` and `Q3 + k*IQR`, with `k = multiplier`.

## Usage

``` r
detect_outlier_thresholds(dataf, var_name, multiplier = 1.5, label = NULL)
```

## Arguments

- dataf:

  A data frame.

- var_name:

  Name of a single numeric column in `dataf`.

- multiplier:

  IQR multiplier. Default `1.5` (Tukey's classic threshold); use `3` for
  "extreme" outliers.

- label:

  Optional label for the `notes` column. Defaults to `var_name`.

## Value

A one-row data frame with columns `LB` (lower bound), `UB` (upper
bound), and `notes`.

## Examples

``` r
df <- data.frame(x = c(1:10, 100))
detect_outlier_thresholds(df, "x")
#>   LB UB notes
#> 1 -4 16     x
detect_outlier_thresholds(df, "x", multiplier = 3)
#>      LB   UB notes
#> 1 -11.5 23.5     x
```
