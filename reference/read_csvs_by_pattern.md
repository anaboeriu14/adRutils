# Read and combine CSV files matching a pattern

Reads all CSV files in a directory whose names match any of the supplied
regex patterns (or all CSVs if `all_files = TRUE`), optionally cleans
column names with
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html),
and either combines them into a single data frame or returns them as a
named list.

## Usage

``` r
read_csvs_by_pattern(
  directory_path,
  missing_vals,
  patterns = NULL,
  all_files = FALSE,
  clean_col_names = TRUE,
  combine = TRUE,
  quiet = FALSE
)
```

## Arguments

- directory_path:

  Path to the directory containing CSV files.

- missing_vals:

  Character vector of strings to treat as `NA`.

- patterns:

  Character vector of regex patterns matching filenames. Required unless
  `all_files = TRUE`.

- all_files:

  If `TRUE`, read every `.csv` in the directory regardless of
  `patterns`. Default `FALSE`.

- clean_col_names:

  If `TRUE`, clean column names with
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html).
  Default `TRUE`.

- combine:

  If `TRUE` (default), bind all files into a single data frame using
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  If `FALSE`, return a named list.

- quiet:

  If `TRUE`, suppress progress messages. Default `FALSE`.

## Value

A combined data frame (when `combine = TRUE`), a named list of data
frames (when `combine = FALSE`), or an empty data frame if no files
matched.
