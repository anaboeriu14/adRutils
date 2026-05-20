# Impute missing data using random forest

Imputes missing values in `vars_to_impute` using
[`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html),
a non-parametric Random Forest imputation method. Helper variables
contribute to the imputation model but are not themselves imputed in the
output.

## Usage

``` r
impute_random_forest(
  dataf,
  vars_to_impute,
  helper_vars,
  parallel = FALSE,
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame.

- vars_to_impute:

  Character vector of columns to impute.

- helper_vars:

  Character vector of additional columns used as predictors during
  imputation. Must not overlap `vars_to_impute`.

- parallel:

  If `TRUE`, use the `"forests"` parallelization mode of
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).
  Default `FALSE`. Requires registering a parallel backend (e.g.,
  `doParallel::registerDoParallel()`) *before* calling.

- quiet:

  If `TRUE`, suppress messages. Default `FALSE`.

## Value

`dataf` with imputed columns appended (named `i_<var>`). The
per-variable out-of-bag imputation error is attached as an attribute
named `"imputation_errors"` (a tibble with `variable` and `MSE`
columns); access it via `attr(result, "imputation_errors")`.

## Details

Imputed columns are added with an `i_` prefix; original columns are
preserved unchanged.

For error estimation,
[`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html)
is given the complete cases of the input as `xtrue`. When few complete
cases are available, the resulting error estimates may be unreliable; a
warning is issued if fewer than 10% of rows are complete.

## Examples

``` r
if (FALSE) { # \dontrun{
# Optional: register a parallel backend before calling with parallel = TRUE
doParallel::registerDoParallel(cores = 3)

result <- impute_random_forest(
  dataf          = mydata,
  vars_to_impute = c("BMI", "glucose"),
  helper_vars    = c("age", "sex", "weight"),
  parallel       = TRUE
)

summary(result$i_BMI)
attr(result, "imputation_errors")
} # }
```
