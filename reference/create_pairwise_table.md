# Create a pairwise group comparison table

Compares groups pairwise using t-tests for numeric variables and
chi-squared tests (with Fisher's exact fallback) for categorical
variables. Returns a tidy table with one row per variable and one column
per group pair.

## Usage

``` r
create_pairwise_table(
  data,
  group_var,
  numeric_vars,
  categorical_vars = NULL,
  p_adjust_method = "bonferroni",
  p_format = c("auto", "threshold", "exact", "scientific", "raw"),
  p_digits = 3
)
```

## Arguments

- data:

  A data frame.

- group_var:

  Name of the grouping variable.

- numeric_vars:

  Character vector of numeric variables to compare via t-tests.

- categorical_vars:

  Character vector of categorical variables. Default `NULL`.

- p_adjust_method:

  P-value adjustment method. Default `"bonferroni"`. See
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  options.

- p_format:

  Display format for p-values. One of `"auto"`, `"threshold"`,
  `"exact"`, `"scientific"`, or `"raw"` (no formatting). Default
  `"auto"`.

- p_digits:

  Decimal places or significant figures (depending on format). Default
  `3`.

## Value

A tibble with `variable`, `test_type`, and one column per group pair.

## Details

Numeric variables use
[`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html)
with the supplied `p_adjust_method`. Categorical variables compute
pairwise tests independently and then adjust via
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) with the
same method.

Pair column names are human-readable (e.g., `"AFR vs EUR"`), so
programmatic access requires backticks: `` result$`AFR vs EUR` ``.

## Examples

``` r
if (FALSE) { # \dontrun{
create_pairwise_table(
  data             = my_data,
  group_var        = "ancestry",
  numeric_vars     = c("age", "bmi"),
  categorical_vars = c("sex", "diagnosis"),
  p_format         = "threshold"
)
} # }
```
