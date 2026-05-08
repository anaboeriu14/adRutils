# Transform numeric variables to a log10 scale

Adds log10-transformed copies of `vars` to `dataf`, prefixed with
`log10_`. Original columns are preserved unchanged.

## Usage

``` r
transform_log10(dataf, vars, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- dataf:

  A data frame.

- vars:

  Character vector of column names to transform. Must be numeric and
  strictly positive.

- overwrite:

  If `TRUE`, replace existing `log10_<var>` columns. Default `FALSE`.

- quiet:

  If `TRUE`, suppress warnings. Default `FALSE`.

## Value

`dataf` with one new column per input variable, named `log10_<var>`.

## Details

Errors when any value in `vars` is `<= 0`, since
[`log10()`](https://rdrr.io/r/base/Log.html) is undefined there. Errors
when a target column (`log10_<var>`) already exists, unless
`overwrite = TRUE`.

Warns (does not error) if an input variable is itself already
`log10_`-prefixed, since this typically indicates double-transformation.

## Examples

``` r
df <- data.frame(x = c(10, 100, 1000), y = c(1, 2, 3))
transform_log10(df, vars = c("x", "y"))
#>      x y log10_x   log10_y
#> 1   10 1       1 0.0000000
#> 2  100 2       2 0.3010300
#> 3 1000 3       3 0.4771213
```
