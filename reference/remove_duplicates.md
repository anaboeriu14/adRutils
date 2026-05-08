# Remove duplicate rows by ID

Identifies and removes duplicate rows based on a single ID column. By
default, keeps the row with the fewest `NA` values per duplicate ID (the
"most complete" row).

## Usage

``` r
remove_duplicates(
  dataf,
  id_col,
  keep = c("most_complete", "first", "last", "none"),
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame.

- id_col:

  Column name (quoted or unquoted) identifying rows.

- keep:

  One of `"most_complete"` (default), `"first"`, `"last"`, or `"none"`.
  `"none"` removes *all* rows with duplicate IDs, including their
  originals.

- quiet:

  If `TRUE`, suppress messages. Default `FALSE`.

## Value

`dataf` with duplicates removed according to `keep`.

## Examples

``` r
df <- data.frame(
  id  = c(1, 2, 2, 3),
  val = c(10, NA, 20, 30)
)
remove_duplicates(df, id_col = "id")  # keeps id=2 row with val=20
#> ℹ Found 1 unique ID with duplicates
#> ℹ Duplicate IDs: "2"
#> ✔ Removed 1 duplicate row
#>   id val
#> 1  1  10
#> 3  2  20
#> 4  3  30
```
