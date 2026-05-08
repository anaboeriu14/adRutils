# Create an ID mapping table from a dataset

Extracts the unique pairs of two ID columns from a dataset, producing a
lookup table suitable for use with
[`add_id_mapping()`](https://anaboeriu14.github.io/adRutils/reference/add_id_mapping.md).

## Usage

``` r
create_id_mapping(data, id_col1, id_col2, trim = TRUE)
```

## Arguments

- data:

  A data frame containing both ID columns.

- id_col1, id_col2:

  Names of the two ID columns. Must differ.

- trim:

  If `TRUE` (default), trim leading/trailing whitespace from the IDs
  before deduplication.

## Value

A data frame with two columns of unique ID pairs.

## Examples

``` r
df <- data.frame(
  short_id = c("A1", "A2", "A1"),
  long_id  = c("subj_001", "subj_002", "subj_001")
)
create_id_mapping(df, "short_id", "long_id")
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `dplyr::across(dplyr::everything(), .normalize_id, trim =
#>   trim)`.
#> Caused by warning:
#> ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
#> Supply arguments directly to `.fns` through an anonymous function instead.
#> 
#>   # Previously
#>   across(a:b, mean, na.rm = TRUE)
#> 
#>   # Now
#>   across(a:b, \(x) mean(x, na.rm = TRUE))
#> ℹ The deprecated feature was likely used in the adRutils package.
#>   Please report the issue at <https://github.com/anaboeriu14/adRutils/issues>.
#> ✔ Created mapping with 2 unique ID pairs
#>   short_id  long_id
#> 1       A1 subj_001
#> 2       A2 subj_002
```
