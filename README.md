# adRutils

[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)](https://github.com/anaboeriu14/adRutils/releases)

R utility functions for data cleaning, transformation, file operations, and statistical modeling

## Overview

`adRutils` provides a collection of general-purpose functions to streamline common data processing tasks and statistical analyses.

## Installation

You can install `adRutils` from GitHub using one of these methods:

### Option 1: Using remotes (recommended)
```r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install adRutils from GitHub
remotes::install_github("anaboeriu14/adRutils")
```

### Option 2: Using devtools
```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("anaboeriu14/adRutils")
```

## Version Information

**Current version:** 1.0.0 (Stable Release)

See [Releases](https://github.com/anaboeriu14/adRutils/releases) & [NEWS.md](NEWS.md) for detailed changelog and release notes.

### What's New in 1.0.0

**First stable release!** The API is now locked down with backward compatibility guarantees.

- **Breaking changes**: Removed tracking system, renamed `force` → `overwrite` parameter
- **Enhanced file reading**: Better handling of inconsistent column types across CSV files
- **Improved duplicate removal**: More robust with rlang, works reliably in pipes
- **Flexible p-value formatting**: Multiple format options for publication-ready tables

## Quick Start
```r
library(adRutils)

# Read and combine CSV files
data <- read_csvs_by_pattern(
  "data/raw",
  missing_vals = c("NA", ""),
  patterns = "baseline"
)

# Clean data
data <- data %>%
  remove_duplicates_if_exists("subject_id") %>%
  drop_sparse_na_cols(threshold = 99) %>%
  transform_log10(vars = c("biomarker1", "biomarker2"))

# Fit models across groups
results <- fit_models_by_group(
  data,
  outcomes = c("log10_biomarker1", "log10_biomarker2"),
  base_predictors = c("age", "sex"),
  group_col = "ancestry",
  groups = c("AFR", "EUR", "EAS")
)

# Pairwise comparisons with flexible formatting
comparison_table <- create_pairwise_table(
  data,
  variables = c("biomarker1", "biomarker2"),
  group_var = "treatment",
  p_format = "threshold"  # or "auto", "exact", "scientific"
)
```

## Documentation

For full documentation, including function details and examples:
```r
# Browse all functions
help(package = "adRutils")

# Function-specific help
?fit_models_by_group
?transform_log10
?create_pairwise_table
```
## License

This project is licensed under the MIT License.

## Citation

If you use this package in your research, please cite:
```
Boeriu, A. (2025). adRutils: R Utility Functions for Data Analysis.
R package version 1.0.0. https://github.com/anaboeriu14/adRutils
```

## Contact

For questions or issues, please open an issue on [GitHub](https://github.com/anaboeriu14/adRutils/issues).
