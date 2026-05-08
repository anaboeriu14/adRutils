# Extract raw coefficients with confidence intervals

Extracts coefficients from a tibble of fitted models (typically the
output of
[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md))
and adds confidence intervals computed from the model standard errors.

## Usage

``` r
extract_coefficients(model_results, term_pattern = NULL, ci_level = 0.95)
```

## Arguments

- model_results:

  A tibble containing a list-column `model_res` of tidy model results
  (as produced by
  [`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)).

- term_pattern:

  Optional regex to filter `term` values.

- ci_level:

  Confidence level for the intervals. Default `0.95`.

## Value

A tibble with one row per coefficient, including columns `estimate`,
`std.error`, `conf.low`, `conf.high`, `p.value`, and `sig` (TRUE when
the CI excludes zero).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- fit_models_by_group(data, outcomes, predictors)
extract_coefficients(results, term_pattern = "treatment")
} # }
```
