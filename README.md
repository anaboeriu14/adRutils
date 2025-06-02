# adRutils

[![Version](https://img.shields.io/badge/version-0.1.2-blue.svg)](https://github.com/anaboeriu14/adRutils/releases)

R utility functions for data cleaning, transformation, and file operations

## Overview

`adRutils` provides a collection of general-purpose functions to streamline common data processing tasks. 

## Version Information

**Current version:** 0.1.1

See [Releases](https://github.com/anaboeriu14/adRutils/releases) for detailed changelog and release notes.

### What's New in 0.1.1
- Function processing tracking system to prevent duplicate operations
- Enhanced transformation functions with processing protection
- Override capabilities for intentional reprocessing

## Installation

You can install the  `adRutils` package from GitHub using one of these methods:

### Option 1: Using remotes (recommended for most users)
The `remotes` package is lightweight and focused solely on package installation:

```r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install adRutils from GitHub
remotes::install_github("anaboeriu14/adRutils")
```
### Option 2: Using devtools 

The `devtools` package is more comprehensive than `remotes`, with additional tools for package development:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("anaboeriu14/adRutils")
```

## Features

This package provides utility functions for data cleaning, transformation, and file operations 

### File Operations
- `read_csvs_by_pattern()`: Read and combine CSV files matching specified patterns

### Data Cleaning & Transformation
- `convert_columns_to_factors()`: Convert specified columns to factor type
- `remove_duplicates_if_exists()`: Identify and remove duplicate observations
- `transform_log10()`: Transform data using log10 with various handling for zeros/negatives

### Missing Value Management
- `summarize_na()`: Generate summaries of missing values in a dataset
- `drop_sparse_na_cols()`: Remove columns with high percentages of missing values
- `rf_impute_data()`: Impute missing values using Random Forest

### Data Selection
- `select_cols_by_pattern()`: Select columns based on name patterns

### Caching
- Functions for caching computation results to improve performance


## Documentation

For full documentation, including function details and additional examples, run:

```r
?adRutils::read_pattern_csv
?adRutils::summarize_na
# etc.
```

## License
This project is licensed under the MIT License
