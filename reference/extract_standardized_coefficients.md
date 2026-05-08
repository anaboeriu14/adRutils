# Extract standardized coefficients

Extracts standardized coefficients from a tibble of fitted models using
[`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html).

## Usage

``` r
extract_standardized_coefficients(
  model_results,
  term_pattern = NULL,
  method = "posthoc"
)
```

## Arguments

- model_results:

  A tibble containing a list-column `model_obj` of fitted model objects
  (as produced by
  [`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)).

- term_pattern:

  Optional regex to filter `Parameter` values.

- method:

  Standardization method passed to
  [`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html).
  Default `"posthoc"`.

## Value

A tibble with standardized coefficients and `sig` indicating whether the
standardized CI excludes zero.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- fit_models_by_group(data, outcomes, predictors)
extract_standardized_coefficients(results, term_pattern = "age")
} # }
```
