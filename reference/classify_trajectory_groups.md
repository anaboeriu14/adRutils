# Classify subjects into trajectory groups from a mixed-effects model

Extracts random slopes from a fitted `lme4` model, z-scores them, and
assigns each subject to a trajectory group (low / mid / high) using
SD-based or quantile-based cutoffs.

## Usage

``` r
classify_trajectory_groups(
  model,
  id_col = "id",
  slope_term = "time",
  methods = c("1sd", "1.5sd", "tertile", "quartile"),
  labels = list(low = "Slow", mid = "Typical", high = "Fast"),
  prefix = "group",
  ref_level = labels$mid
)
```

## Arguments

- model:

  A fitted `lmerMod` object with a random slope term.

- id_col:

  Output ID column name. Default `"id"`.

- slope_term:

  Name of the random slope variable (must match a column of
  `ranef(model)`). Default `"time"`.

- methods:

  Character vector. Any subset of `"1sd"`, `"1.5sd"`, `"tertile"`,
  `"quartile"`.

- labels:

  Named list with elements `low`, `mid`, `high`.

- prefix:

  Prefix for output group columns. Default `"group"`.

- ref_level:

  Reference level for the output factor. Default `labels$mid`.

## Value

A tibble with one row per random-effects group, containing `<id_col>`,
`random_intercept` (if present), `random_slope`, `slope_z`, and one
factor column per requested method, named `<prefix>_<method>`.

## Details

SD-based methods (`"1sd"`, `"1.5sd"`) threshold the z-scored slope:
subjects beyond ±1 (or ±1.5) SD from the group mean are assigned to the
low/high groups, the rest to mid.

Quantile-based methods (`"tertile"`, `"quartile"`) threshold the raw
slope: the lowest tertile (or quartile) goes to `low`, the highest to
`high`, the rest to `mid`. Subjects exactly at a cutpoint are assigned
to the middle group.

The `mid` label is the default reference level since it represents the
typical trajectory; most downstream contrasts compare extreme groups to
the typical one.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- lme4::lmer(score ~ time + (1 + time | id), data = long_df)
classify_trajectory_groups(fit, id_col = "subject_id")

# Custom labels
classify_trajectory_groups(
  fit,
  labels = list(low = "Decliner", mid = "Stable", high = "Improver"),
  prefix = "cog_traj"
)
} # }
```
