# Upgrading from 1.x to 2.0

Version 2.0.0 is a cleanup release. Several function and parameter names
changed for consistency, the validation framework was rewritten, and a
few utilities moved to other packages or were inlined into the functions
that used them. This guide walks through what changed and how to update
your code.

If you only used a handful of functions, the [Quick
reference](#quick-reference) at the end is probably all you need.

## Why the changes

Three goals drove the 2.0 cleanup:

1.  **Consistent naming** across the package. Verbs like `quiet`
    (suppress messages) and nouns like `classify_trajectory_groups` (do
    what the function does) replace older mixed conventions (`verbose`,
    `categorize_slopes`).
2.  **A single validation interface.** The old `validate_params` worked
    but couldn’t be extended cleanly. `validate_args` and the exported
    `is_*()` assertion helpers replace it with composable pieces.
3.  **Tighter scope.** Caching utilities and outlier detection moved to
    the packages where they actually belong; meta-analysis weights are
    now computed automatically rather than as a separate step.

## Renamed functions

### `categorize_slopes()` → `classify_trajectory_groups()`

The new name describes what the function returns (trajectory group
memberships) rather than the operation on the input. The signature is
otherwise compatible.

``` r

# Before (1.x)
categorize_slopes(model, id_col = "subject_id")

# After (2.0)
classify_trajectory_groups(model, id_col = "subject_id")
```

### `coalesce_timepoints()` → `combine_timepoints()`

Renamed to avoid collision with
[`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)
and to make the intent (combining longitudinal visits) clearer.

``` r

# Before
coalesce_timepoints(df, base_names = "dsst", timepoints = c("30", "35"))

# After
combine_timepoints(df, base_names = "dsst", timepoints = c("30", "35"))
```

### `extract_standardized_coefs()` → `extract_standardized_coefficients()`

Spelled out for symmetry with
[`extract_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_coefficients.md).

### `remove_duplicates_if_exists()` → `remove_duplicates()`

The `_if_exists` suffix was misleading — the function always ran; it
just didn’t fail when there were no duplicates. The new name is what it
does.

### `validate_params()` → `validate_args()`

Rewritten with a richer interface. See [Validation
framework](#validation-framework) below.

## Renamed parameters

| Function | Old | New |
|----|----|----|
| [`transform_log10()`](https://anaboeriu14.github.io/adRutils/reference/transform_log10.md) | `force` | `overwrite` |
| [`coalesce_variables()`](https://anaboeriu14.github.io/adRutils/reference/coalesce_variables.md) | `force` | `overwrite` |
| Most functions | `verbose = TRUE` | `quiet = FALSE` |

The flip from `verbose` to `quiet` means the **default behavior is
unchanged** (messages still print by default), but the flag is inverted.
If you were passing `verbose = FALSE` to silence output, switch to
`quiet = TRUE`.

``` r

# Before
transform_log10(df, vars = "x", force = TRUE)
fit_models_by_group(data, outcomes, predictors, verbose = FALSE)

# After
transform_log10(df, vars = "x", overwrite = TRUE)
fit_models_by_group(data, outcomes, predictors, quiet = TRUE)
```

## Validation framework

`validate_params()` was replaced with
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md),
which has a cleaner interface and a set of exported assertion helpers
you can use in your own code.

``` r

# Before
validate_params(
  data = dataf,
  required_cols = vars,
  numeric_cols  = vars,
  logical_args  = list(quiet = quiet, overwrite = overwrite)
)

# After
validate_args(
  data            = dataf,
  columns         = vars,
  numeric_columns = vars,
  vars            = is_nonempty_character(),
  overwrite       = is_flag(),
  quiet           = is_flag()
)
```

Available assertion helpers:
[`is_flag()`](https://anaboeriu14.github.io/adRutils/reference/is_flag.md),
[`is_string()`](https://anaboeriu14.github.io/adRutils/reference/is_string.md),
[`is_nonempty_character()`](https://anaboeriu14.github.io/adRutils/reference/is_nonempty_character.md),
[`is_nonempty_list()`](https://anaboeriu14.github.io/adRutils/reference/is_nonempty_list.md),
[`is_number()`](https://anaboeriu14.github.io/adRutils/reference/is_number.md),
[`is_one_of()`](https://anaboeriu14.github.io/adRutils/reference/is_one_of.md).
Custom checks still work via the `custom_checks` argument.

## Removed functions

### Caching utilities

The cache system (`init_cache()`, `cache_get()`, `cache_set()`, etc.)
moved to the `adRpheno` package, where it’s used. For general-purpose
caching, the `memoise` and `cachem` packages are well-maintained
alternatives.

``` r

# Before
result <- cache_get("expensive_calc")
if (is.null(result)) {
  result <- expensive_calc(data)
  cache_set("expensive_calc", result)
}

# After (with memoise)
expensive_calc_cached <- memoise::memoise(expensive_calc)
result <- expensive_calc_cached(data)
```

### `add_inverse_variance_weights()`

Removed. REML weights are now computed automatically inside
[`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)
and attached as a `weight` column on each cohort row.

``` r

# Before
data %>%
  add_inverse_variance_weights() %>%
  add_meta_pooled_results(group_vars = c("outcome", "term"))

# After
data %>%
  add_meta_pooled_results(group_vars = c("outcome", "term"))
# `weight` column is now part of the output
```

### `fit_single_lm()`

No longer exported; the logic is internal to
[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md).
To fit a single model, call
[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
with a single outcome and the default `groups = "All"`.

``` r

# Before
fit_single_lm(data, outcome = "y", predictors = c("age", "sex"))

# After
fit_models_by_group(
  data,
  outcomes        = "y",
  base_predictors = c("age", "sex")
)
```

### `extract_pairwise_pvalues()`

Folded into
[`create_pairwise_table()`](https://anaboeriu14.github.io/adRutils/reference/create_pairwise_table.md),
which now handles both numeric and categorical comparisons in one call.
See the [function
reference](https://anaboeriu14.github.io/adRutils/reference/create_pairwise_table.md)
for the new signature.

### Tracking system

`tracking.R` (column-existence tracking) was removed in 1.0. If you
still have references, replace them with direct `%in% names(df)` checks.

## Moved to other packages

### Outlier detection — moved *into* adRutils

[`detect_outlier_thresholds()`](https://anaboeriu14.github.io/adRutils/reference/detect_outlier_thresholds.md)
and
[`replace_outliers_with_na()`](https://anaboeriu14.github.io/adRutils/reference/replace_outliers_with_na.md)
moved here from `adRpheno` since they’re domain-agnostic. If you
previously loaded `adRpheno` only for outlier detection, you can now use
`adRutils` alone.

## Renamed columns in output

### Confidence intervals: `lower_ci` / `upper_ci` → `conf.low` / `conf.high`

The meta-analysis and coefficient-extraction functions now use the broom
convention. If you have downstream code referencing the old names:

``` r

# Before
results %>% filter(lower_ci > 0)

# After
results %>% filter(conf.low > 0)
```

## Bug fixes worth knowing about

These are listed in NEWS.md but are worth flagging here because they
change *behavior*, not just APIs:

- **[`compare_coefs()`](https://anaboeriu14.github.io/adRutils/reference/compare_coefs.md)**
  now uses `2 * pnorm(-abs(z))` instead of `2 * (1 - pnorm(abs(z)))`.
  P-values for highly significant differences (\|z\| \> ~8) that
  previously underflowed to 0 are now computed accurately.
- **[`bin_and_categorize_variables()`](https://anaboeriu14.github.io/adRutils/reference/bin_and_categorize_variables.md)**
  applies categorical mappings in a single pass. If you had a mapping
  like `c("A" = "B", "B" = "C")`, the old version may have chained the
  remap (`A → B → C`); the new version does not (`A → B`, `B → C`).
- **[`convert_columns_to_factors()`](https://anaboeriu14.github.io/adRutils/reference/convert_columns_to_factors.md)**
  preserves unobserved factor levels when promoting an existing factor
  to ordered.

If any of these match a pattern in your code, double-check the output
after upgrading.

## Quick reference

| Old (1.x) | New (2.0) |
|----|----|
| `categorize_slopes()` | [`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md) |
| `coalesce_timepoints()` | [`combine_timepoints()`](https://anaboeriu14.github.io/adRutils/reference/combine_timepoints.md) |
| `extract_standardized_coefs()` | [`extract_standardized_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_standardized_coefficients.md) |
| `remove_duplicates_if_exists()` | [`remove_duplicates()`](https://anaboeriu14.github.io/adRutils/reference/remove_duplicates.md) |
| `validate_params()` | [`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md) |
| `force = TRUE` | `overwrite = TRUE` |
| `verbose = FALSE` | `quiet = TRUE` |
| `lower_ci`, `upper_ci` | `conf.low`, `conf.high` |
| `add_inverse_variance_weights()` | (automatic in [`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)) |
| `fit_single_lm()` | [`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md) with one outcome |
| `extract_pairwise_pvalues()` | [`create_pairwise_table()`](https://anaboeriu14.github.io/adRutils/reference/create_pairwise_table.md) |
| `cache_get()`, `cache_set()`, etc. | `memoise` or `cachem` package |

## Getting help

If you hit a migration issue not covered here, please [open an
issue](https://github.com/anaboeriu14/adRutils/issues) with a minimal
reproducible example. For the full release notes, see
[NEWS.md](https://github.com/anaboeriu14/adRutils/blob/main/NEWS.md).
