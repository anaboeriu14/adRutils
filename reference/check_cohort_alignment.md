# Check that two dataframes have matching key combinations

Compares the set of key combinations between two dataframes (typically a
discovery and replication cohort) and reports any combinations present
in only one. Useful as a pre-flight check before joining or
meta-analyzing.

## Usage

``` r
check_cohort_alignment(x, y, by, x_label = "x", y_label = "y", quiet = FALSE)
```

## Arguments

- x, y:

  Dataframes to compare.

- by:

  Character vector of column names defining the key.

- x_label, y_label:

  Labels used in messages. Default `"x"` and `"y"`.

- quiet:

  If `TRUE`, suppress CLI output. Default `FALSE`.

## Value

Invisibly, a list with elements `x_only`, `y_only`, and `perfect_match`
(logical).
