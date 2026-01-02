# adRutils 1.0.0

## Breaking Changes

If upgrading from 0.4.0 or earlier, note these changes:

**Removed Systems** - **Tracking system** (`tracking.R`) - check for column existence directly - **Cache system** (`cache.R`) - implement at script level if needed

**Parameter Renames** - `transform_log10()`: `force` → `overwrite` - `coalesce_variables()`: `force` → `overwrite`

``` r
# Migration example
# Before: transform_log10(data, vars = "x", force = TRUE)
# After:  transform_log10(data, vars = "x", overwrite = TRUE)
```

## New Features

**Pairwise Comparisons** - Flexible p-value formatting: `auto`, `threshold`, `exact`, `scientific` - Functions: `extract_pairwise_pvalues()`, `create_pairwise_table()`

**Model Enhancements** - `fit_models_by_group()` supports ungrouped analysis (`group_col = NULL`) - Access both tidy results (`model_res`) and raw model object (`model_obj`)

**Better Duplicate Handling** - `remove_duplicates_if_exists()` now uses rlang - Works with quoted (`"med_id"`) and unquoted (`med_id`) syntax

**Improved File Reading** - `read_csvs_by_pattern()` has better error messages for type mismatches - Suggests solutions when column types conflict across files

## Bug Fixes

-   Fixed parameter mismatch in `convert_columns_to_factors()`
-   Fixed column extraction in `remove_duplicates_if_exists()`
-   Fixed type coercion issues in `read_csvs_by_pattern()`
-   Cleaned up orphaned helper functions

## Stability Guarantee

**Semantic Versioning:** 
- `1.x.0` New features, backward compatible 
- `1.0.x`  Bug fixes only 
- `2.0.0`  Breaking changes

------------------------------------------------------------------------

# adRutils 0.4.0

**New:** Enhanced model output

-   `fit_models_by_group()` supports `group_col = NULL` for ungrouped analysis
-   Added `model_obj` column for direct model access

------------------------------------------------------------------------

# adRutils 0.3.0

**New:** Group-specific covariates, pairwise comparisons

-   Enhanced `fit_models_by_group()` with group-specific outcome covariates
-   `fit_single_lm()` for individual model fitting
-   `extract_pairwise_pvalues()` and `create_pairwise_table()`

------------------------------------------------------------------------

# adRutils 0.2.0

**New:** Variable coalescing, validation framework

-   `coalesce_variables()` - Combine variables by pattern or manual grouping
-   `validate_params()` - Comprehensive input validation

------------------------------------------------------------------------

# adRutils 0.1.2

**Fixed:** `remove_duplicates_if_exists()` error with `keep = "most_complete"`

------------------------------------------------------------------------

# adRutils 0.1.1

**New:** Processing tracking system (removed in 1.0.0)

------------------------------------------------------------------------

# adRutils 0.1.0

**Initial release:** File operations, missing data tools, transformations
