# Run REML meta-analysis within groups and append pooled rows

For each group defined by `group_vars`, fits a random-effects
meta-analysis via
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
with `method = "REML"` and appends a pooled row to the data. Each cohort
row receives a `weight` column giving its REML contribution (in percent)
to the pooled estimate; pooled rows have `weight = NA`.

## Usage

``` r
add_meta_pooled_results(
  data,
  group_vars,
  cohort_col = "cohort",
  discovery_label = NULL,
  replication_label = NULL,
  pooled_label = "Pooled",
  cohort_levels = NULL
)
```

## Arguments

- data:

  A tidy effects data frame containing at minimum `cohort`, `estimate`,
  `std.error`, and `n_obs` columns.

- group_vars:

  Character vector of grouping columns (e.g., `c("outcome", "term")`).

- cohort_col:

  Name of the cohort identifier column. Default `"cohort"`.

- discovery_label, replication_label:

  Labels identifying the discovery and replication cohorts, used only
  when computing `same_direction` and `replicated` for k = 2 groups. If
  `NULL` (default), the first and second cohort values (in appearance
  order) are used.

- pooled_label:

  Label used for the appended pooled row. Default `"Pooled"`.

- cohort_levels:

  Optional character vector defining factor levels for the `cohort`
  column in the returned data. If `NULL`, a factor is built from the
  unique values with `pooled_label` last.

## Value

`data` with pooled rows appended and a `weight` column added. New
columns on the pooled rows: `I2`, `Q`, `tau2`, `n_cohorts`,
`same_direction`, `replicated`. The pooled row's `n_obs` is the sum
across cohorts (total subjects, not effective sample size).

## Details

Heterogeneity statistics (`I2`, `Q`, `tau2`) are attached to the pooled
row when k \>= 2.

When exactly two cohorts are present in a group, `same_direction` (sign
agreement) and `replicated` (sign agreement AND \|β/SE\| \> 1.96 in the
second cohort) are computed for the pooled row. These concepts do not
generalize to k \>= 3, so they are set to `NA` in that case.

Pooling requires at least 2 cohorts in `data` overall; an input with
only one cohort is treated as a misuse and aborts immediately.

Within a multi-cohort dataset, individual `(group_vars)` cells may still
have only one cohort represented (e.g., when a model failed to fit in
the other cohort, or when the input is unbalanced). These cells are
retained as-is, with no pooled row appended and `weight = NA`, and a
warning identifies which cells were skipped.

Confidence intervals for the pooled row are computed from the REML model
and stored in `conf.low`/`conf.high` (broom convention). If your input
data uses other CI column names (e.g., `lci`/`uci`), those columns will
be treated as metadata and carried forward unchanged from the first
cohort row of each group, NOT recomputed. Rename to
`conf.low`/`conf.high` before calling to get correct pooled CIs.
