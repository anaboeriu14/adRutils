# Combine measures across longitudinal visits

For each base variable, selects the first non-missing value across a
priority-ordered sequence of timepoint suffixes. Creates two columns per
base variable: a coalesced value and a source label indicating which
timepoint contributed.

## Usage

``` r
combine_timepoints(
  data,
  base_names,
  timepoints,
  final_suffix = "_final",
  source_suffix = "_source",
  timepoint_labels = NULL
)
```

## Arguments

- data:

  A data frame.

- base_names:

  Character vector of variable base names (e.g., `"dsst"`, `"ravlt"`).

- timepoints:

  Character vector of suffixes in priority order; the first entry is the
  preferred source.

- final_suffix:

  Suffix for the coalesced value column. Default `"_final"`.

- source_suffix:

  Suffix for the source-tracking column. Default `"_source"`.

- timepoint_labels:

  Optional named character vector mapping suffixes to display labels. If
  `NULL`, suffixes are used verbatim.

## Value

`data` with `<base>_final` and `<base>_source` columns added for each
base name that has at least one matching timepoint column.

## Examples

``` r
df <- data.frame(
  dsst30  = c(45, NA, 38),
  dsst35  = c(NA, 52, 41),
  ravlt30 = c(NA, 30, NA),
  ravlt35 = c(28, 32, 35)
)
combine_timepoints(
  df,
  base_names = c("dsst", "ravlt"),
  timepoints = c("30", "35")
)
#>   dsst30 dsst35 ravlt30 ravlt35 dsst_final dsst_source ravlt_final ravlt_source
#> 1     45     NA      NA      28         45          30          28           35
#> 2     NA     52      30      32         52          35          30           30
#> 3     38     41      NA      35         38          30          35           35
```
