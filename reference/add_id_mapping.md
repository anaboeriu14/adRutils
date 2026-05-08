# Add an ID column via lookup

Joins an ID mapping table to `data`, adding `id_col_to_add` based on
matches in `existing_id_col`. If `id_col_to_add` already exists in
`data`, it is replaced.

## Usage

``` r
add_id_mapping(
  data,
  id_mapping,
  existing_id_col,
  id_col_to_add,
  trim = TRUE,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame.

- id_mapping:

  A lookup table containing both `existing_id_col` and `id_col_to_add`.

- existing_id_col:

  Name of the ID column already in `data`.

- id_col_to_add:

  Name of the ID column to add from `id_mapping`.

- trim:

  If `TRUE` (default), trim whitespace before matching.

- quiet:

  If `TRUE`, suppress matching statistics. Default `FALSE`.

## Value

`data` with `id_col_to_add` added (or replaced).
