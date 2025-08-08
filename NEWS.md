# adRutils 0.4.0

## New Features - Interaction plotting functions

- `create_interaction_plots()`  - Generates interaction plots from model results, 
featuring customizable colors, themes, and optional confidence bands with
significance highlighting.

- `organize_interaction_plots()` - arranges interaction plots into publication-ready grids with 
flexible ordering by outcomes and numeric variables, subset selection, and automatic legend collection and panel labeling

-  `generate_interaction_plots()`  - streamlines the entire process by combining 
plot creation and organization into a single pipeline function.  

## Enhancements

* **Enhanced model output**: Added `model_obj` column to `fit_single_lm()` and `fit_models_by_group()` 
for direct model access

## Changes
* Interaction plotting functions are designed to work seamlessly with `fit_models_by_group()` output
* Model fitting functions now provide both tidy results (`model_res`) and raw model objects (`model_obj`)

# adRutils 0.3.0

## New Features & Improvements

###  Statistical Modeling

  - **Enhanced `fit_models_by_group()`**  function with major improvements:
    - **Group-specific outcome covariates**: Support for different covariates per group (e.g., g1 vs g2 groups)
    - **Automatic structure detection**:Handles both standard and group-specific covariate specifications
    - **Backward compatible**: Existing code continues to work unchanged
    - **Clearer scope**: Enhanced documentation emphasizing linear model (lm) focus
    
  - **New `fit_single_lm()`** exported function for fitting individual linear models
    - Standalone function for single outcome-group combinations
    - Same return format as main function for consistency
    - Useful for custom modeling workflows

### Pairwise Comparison Functions

  - `extract_pairwise_pvalues()` - Extract and format p-values from pairwise t-tests for individual variables
    - Performs pairwise t-tests between groups with formatted p-value output
    - Works with any categorical grouping variable (not limited to specific group names)
    - Returns formatted p-values: "< 0.001" for very small values, rounded to 3 decimals otherwise

  - `create_pairwise_table()` - Create tables with pairwise comparisons for multiple variables
    - Processes multiple numeric variables at once against a grouping variable
    - Returns results in wide table format suitable for reports and manuscripts
    - Built on top of extract_pairwise_pvalues() for consistency


## Changes
* Added internal helper functions `.build_predictors()` and `.fit_single_model()` for `fit_group_models()`

## Breaking Changes
* None

# adRutils 0.2.0

## New Features

* Added `coalesce_variables()` function for combining related variables from different time points or sources
  - Supports pattern-based grouping (e.g., "_t1", "_t2" → combined columns)
  - Supports custom grouping for precise control over which columns to combine

* Added `validate_params()` function for comprehensive input validation
  - Validates data frames, column existence, numeric columns, and grouping variables
  - Supports method validation and custom checks
  - Provides consistent, context-aware error messages
  - Designed to reduce code duplication across package functions

# adRutils 0.1.2

## Bug Fixes
* Fixed error in `remove_duplicates_if_exists()` when using `keep = "most_complete"`
  - Resolved "invalid 'type' (list) of argument" error in `min(na_counts)`
  - Fixed "no non-missing arguments to min" warning when processing NA IDs
  - Replaced `sapply()` with `vapply()` 
  
# adRutils 0.1.1

## New Features
* Added function processing tracking system to prevent duplicate operations
  * `is_processed()`: Check if variables have been processed by a function
  * `register_processed()`: register variables as processed
  * `reset_processing()`: Reset processing history
* Enhanced `transform_log10()` with processing tracking to prevent accidental double transformations
* Added override capability with `force` parameter to intentionally reprocess data when needed

# adRutils 0.1.0

## Initial Release

### New Features
* Added file operations with `read_csvs_by_pattern()`
* Implemented missing value management with `summarize_na()` and `drop_na_sparse_cols()`
* Created data transformation utilities: `convert_columns_to_factors()`, `transform_log10()`
* Added data selection with `select_cols_by_pattern()`
* Developed caching system for improved performance

### Bug Fixes
* None (initial release)

### Breaking Changes
* None (initial release)
