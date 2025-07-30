# adRutils 0.3.0

## New Features & Improvements
* Added `fit_group_models()` function for fitting linear models across multiple outcomes and/or groups
  - Supports flexible covariate specifications with outcome-specific and group-specific adjustments
  - Returns tidy results with model objects, summaries, and diagnostics
  - Supports both main and interaction models

## Changes
* Added internal helper functions `.build_predictors()` and `.fit_single_model()`

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
