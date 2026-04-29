# adRutils

[![Version](https://img.shields.io/badge/version-2.0.0-blue.svg)](https://github.com/anaboeriu14/adRutils/releases) [![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A collection of R utility functions for data cleaning, transformation, file operations, ID mapping, and statistical modeling. Designed to streamline data workflows and improve reproducibility.

> **Version 2.0.0 is a major cleanup release.** Several functions and parameters have been renamed for clarity and consistency. See [NEWS.md](NEWS.md) for the full list.

## Installation

Install from GitHub using one of these methods:

### Option 1: Using remotes (recommended)

``` r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install adRutils from GitHub
remotes::install_github("anaboeriu14/adRutils")
```

### Option 2: Using devtools

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("anaboeriu14/adRutils")
```

## Quick Start

``` r
library(adRutils)

# Read and combine CSV files
data <- read_csvs_by_pattern(
  "data/raw",
  missing_vals = c("NA", ""),
  patterns     = "baseline"
)

# Clean data
data <- data %>%
  remove_duplicates("subject_id") %>%
  drop_high_na_cols(threshold = 99) %>%
  transform_log10(vars = c("biomarker1", "biomarker2"))

# Fit models across groups
results <- fit_models_by_group(
  data,
  outcomes        = c("log10_biomarker1", "log10_biomarker2"),
  base_predictors = c("age", "sex"),
  group_col       = "ancestry",
  groups          = c("AFR", "EUR", "EAS")
)

# Pairwise comparisons with flexible formatting
comparison_table <- create_pairwise_table(
  data,
  numeric_vars     = c("biomarker1", "biomarker2"),
  group_var        = "treatment",
  p_format         = "threshold"
)
```

## Documentation

For full documentation, including function details and examples:

``` r
# Browse all functions
help(package = "adRutils")

# Function-specific help
?fit_models_by_group
?transform_log10
?create_pairwise_table
```

## Version Information

**Current version:** 2.0.0

See [Releases](https://github.com/anaboeriu14/adRutils/releases) and [NEWS.md](NEWS.md) for the full changelog.

## License

This project is licensed under the MIT License.

## Citation

If you use this package in your research, please cite:

```         
Boeriu, A.I. (2026). adRutils: R Utility Functions for Data Analysis.
R package version 2.0.0. https://github.com/anaboeriu14/adRutils
```

## Contact

For questions or issues, please open an issue on [GitHub](https://github.com/anaboeriu14/adRutils/issues).
