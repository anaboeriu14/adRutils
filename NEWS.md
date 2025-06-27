# adRutils 0.2.0

## New Features

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
