# Fit linear models across outcomes and groups

Fits a linear model for every combination of outcome and group, with
optional outcome-specific or group-specific covariates. Returns a tibble
with one row per model, including the fitted model object, tidy results,
and the data subset used.

## Usage

``` r
fit_models_by_group(
  data,
  outcomes,
  base_predictors,
  group_col = NULL,
  groups = "All",
  model_type = "main",
  interaction_terms = NULL,
  outcome_covariates = NULL,
  group_covariates = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing all variables referenced.

- outcomes:

  Character vector of outcome variable names.

- base_predictors:

  Character vector of predictors included in every model.

- group_col:

  Column name for grouping. Use `NULL` for ungrouped analysis
  (`groups = "All"`).

- groups:

  Character vector of group values to analyze. Default `"All"` runs on
  the full dataset.

- model_type:

  One of `"main"` or `"interaction"`.

- interaction_terms:

  Used when `model_type = "interaction"`. Either `"all_pairwise"` (all
  two-way interactions among predictors) or a character string of
  explicit interaction terms.

- outcome_covariates:

  Optional named list. Two supported shapes:

  - Outcome-specific: `list(outcome1 = c("cov1", "cov2"))`. The named
    covariates are added to models for that outcome, across all groups.

  - Group-and-outcome-specific:
    `list(group1 = list(outcome1 = c("cov1")))`. The shape is detected
    automatically.

- group_covariates:

  Optional named list of group-specific covariates, e.g.,
  `list(group1 = c("cov1"), group2 = c("cov2"))`.

- quiet:

  If `TRUE`, suppress progress messages. Default `FALSE`.

## Value

A tibble with columns: `outcome`, the grouping column (named after
`group_col` or `"group"`), `model`, `predictors`, `model_equation`,
`model_res` (tidy results, list-column), `model_obj` (fitted model,
list-column), `model_error` (error message if fit failed, otherwise
`NA`), `dataf` (data subset used, list-column), and `n_obs`.

## Examples

``` r
if (FALSE) { # \dontrun{
fit_models_by_group(
  data            = my_data,
  outcomes        = c("memory", "attention"),
  base_predictors = c("age", "sex"),
  group_col       = "ancestry",
  groups          = c("AFR", "EUR")
)
} # }
```
