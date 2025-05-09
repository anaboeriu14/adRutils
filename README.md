# adRUtils
R utility functions for data cleaning, transformation, and file operations

## Overview

`adRUtils` provides a collection of general-purpose functions to streamline common data processing tasks. 

## Installation

You can install the development version of `adRUtils` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("anaboeriu14/adRUtils")
```

## Features

This package provides utility functions for:

### File Operations
- `read_csvs_by_pattern()`: Read and combine CSV files matching specified patterns

### Data Cleaning & Transformation
- `convert_columns_to_factors()`: Convert specified columns to factor type
- `remove_duplicates_if_exists()`: Identify and remove duplicate observations
- `transform_log10()`: Transform data using log10 with various handling for zeros/negatives

### Missing Value Management
- `summarize_na()`: Generate summaries of missing values in a dataset
- `drop_na_sparse_cols()`: Remove columns with high percentages of missing values
- `rf_impute_data()`: Impute missing values using Random Forest

### Data Selection
- `select_cols_by_pattern()`: Select columns based on name patterns

### Caching
- Functions for caching computation results to improve performance


## Documentation

For full documentation, including function details and additional examples, run:

```r
?adRUtils::read_pattern_csv
?adRUtils::summarize_na
# etc.
```

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
