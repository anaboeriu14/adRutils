# Package index

## Package overview

- [`adRutils`](https://anaboeriu14.github.io/adRutils/reference/adRUtils.md)
  : adRutils: A collection of R utility functions

## Data cleaning

Detect and remove problematic values, duplicate rows, and missing data
before downstream analysis.

- [`detect_outlier_thresholds()`](https://anaboeriu14.github.io/adRutils/reference/detect_outlier_thresholds.md)
  : Detect outlier thresholds via the IQR rule

- [`replace_outliers_with_na()`](https://anaboeriu14.github.io/adRutils/reference/replace_outliers_with_na.md)
  :

  Replace outliers in numeric variables with `NA`

- [`remove_duplicates()`](https://anaboeriu14.github.io/adRutils/reference/remove_duplicates.md)
  : Remove duplicate rows by ID

- [`summarize_na()`](https://anaboeriu14.github.io/adRutils/reference/summarize_na.md)
  : Summarize NA values by column

- [`drop_high_na_cols()`](https://anaboeriu14.github.io/adRutils/reference/drop_high_na_cols.md)
  : Drop columns with a high percentage of NA values

## Variable transformation

Reshape, recode, and combine variables. Most functions add new columns
rather than replacing originals, so transformations are easy to inspect
and undo.

- [`transform_log10()`](https://anaboeriu14.github.io/adRutils/reference/transform_log10.md)
  : Transform numeric variables to a log10 scale
- [`bin_and_categorize_variables()`](https://anaboeriu14.github.io/adRutils/reference/bin_and_categorize_variables.md)
  : Bin and categorize variables into groups
- [`coalesce_variables()`](https://anaboeriu14.github.io/adRutils/reference/coalesce_variables.md)
  : Coalesce related variables into a single column
- [`combine_timepoints()`](https://anaboeriu14.github.io/adRutils/reference/combine_timepoints.md)
  : Combine measures across longitudinal visits
- [`convert_columns_to_factors()`](https://anaboeriu14.github.io/adRutils/reference/convert_columns_to_factors.md)
  : Convert columns to factors based on name patterns
- [`select_cols_by_pattern()`](https://anaboeriu14.github.io/adRutils/reference/select_cols_by_pattern.md)
  : Select columns by regex pattern

## ID mapping

Build and apply ID lookup tables when datasets share subjects but use
different identifier formats.

- [`create_id_mapping()`](https://anaboeriu14.github.io/adRutils/reference/create_id_mapping.md)
  : Create an ID mapping table from a dataset
- [`add_id_mapping()`](https://anaboeriu14.github.io/adRutils/reference/add_id_mapping.md)
  : Add an ID column via lookup

## Imputation

- [`impute_random_forest()`](https://anaboeriu14.github.io/adRutils/reference/impute_random_forest.md)
  : Impute missing data using random forest

## Modeling

Fit linear models and extract coefficients across outcomes and groups,
with support for outcome-specific and group-specific covariates.

- [`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
  : Fit linear models across outcomes and groups
- [`extract_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_coefficients.md)
  : Extract raw coefficients with confidence intervals
- [`extract_standardized_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_standardized_coefficients.md)
  : Extract standardized coefficients
- [`compare_coefs()`](https://anaboeriu14.github.io/adRutils/reference/compare_coefs.md)
  : Compare two coefficients via a z-score test

## Meta-analysis

REML-based pooling across cohorts, with replication-style indicators for
two-cohort designs.

- [`check_cohort_alignment()`](https://anaboeriu14.github.io/adRutils/reference/check_cohort_alignment.md)
  : Check that two dataframes have matching key combinations
- [`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)
  : Run REML meta-analysis within groups and append pooled rows

## Longitudinal & mixed-effects

- [`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
  : Classify subjects into trajectory groups from a mixed-effects model

## Group comparisons

- [`create_pairwise_table()`](https://anaboeriu14.github.io/adRutils/reference/create_pairwise_table.md)
  : Create a pairwise group comparison table

## File I/O & paths

- [`read_csvs_by_pattern()`](https://anaboeriu14.github.io/adRutils/reference/read_csvs_by_pattern.md)
  : Read and combine CSV files matching a pattern
- [`find_sibling_repo_path()`](https://anaboeriu14.github.io/adRutils/reference/find_sibling_repo_path.md)
  : Locate a sibling repository's path

## Argument validation

A consistent interface for input validation, used throughout the package
and available for downstream code.

- [`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
  : Validate function inputs with consistent error messages

- [`custom_check()`](https://anaboeriu14.github.io/adRutils/reference/custom_check.md)
  : Build a lazily-evaluated custom validation check

- [`new_assertion()`](https://anaboeriu14.github.io/adRutils/reference/new_assertion.md)
  : Create a new validation assertion

- [`is_flag()`](https://anaboeriu14.github.io/adRutils/reference/is_flag.md)
  :

  Assert a single logical value (`TRUE` or `FALSE`, not `NA`)

- [`is_string()`](https://anaboeriu14.github.io/adRutils/reference/is_string.md)
  : Assert a single non-empty string

- [`is_nonempty_character()`](https://anaboeriu14.github.io/adRutils/reference/is_nonempty_character.md)
  : Assert a non-empty character vector

- [`is_nonempty_list()`](https://anaboeriu14.github.io/adRutils/reference/is_nonempty_list.md)
  : Assert a non-empty list

- [`is_number()`](https://anaboeriu14.github.io/adRutils/reference/is_number.md)
  : Assert a single numeric value, optionally bounded

- [`is_one_of()`](https://anaboeriu14.github.io/adRutils/reference/is_one_of.md)
  : Assert one of a fixed set of values
