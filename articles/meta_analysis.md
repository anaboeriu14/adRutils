# Meta-analysis workflow

This vignette walks through a full meta-analysis pipeline in `adRutils`:
aligning two cohorts, fitting parallel models in each, extracting tidy
coefficients, and pooling effects via REML. The functions involved are
designed to work together seamlessly, but you might not see that from
any single reference page — the goal here is to show the pipeline end to
end on a small synthetic example.

``` r

library(adRutils)
library(dplyr)
```

## A two-cohort design

We’ll simulate a discovery cohort and a replication cohort, each with
the same outcomes and predictors, but with slightly different sample
sizes and effect magnitudes. This mirrors a common pattern in genetic
and biomarker studies: estimate effects in one population, then test
whether they hold in another.

``` r

set.seed(42)

simulate_cohort <- function(n, cohort_label, effect_age, effect_treatment) {
  tibble::tibble(
    subject_id = paste0(cohort_label, "_", seq_len(n)),
    cohort     = cohort_label,
    age        = round(rnorm(n, mean = 65, sd = 8), 1),
    sex        = sample(c("F", "M"), n, replace = TRUE),
    treatment  = factor(sample(c("control", "active"), n, replace = TRUE),
                        levels = c("control", "active")),
    nfl = effect_age * scale(age)[, 1] +
                  effect_treatment * (treatment == "active") +
                  rnorm(n, sd = 1),
    ab42 = 0.5 * scale(age)[, 1] +
                  0.2 * (treatment == "active") +
                  rnorm(n, sd = 1)
  )
}

discovery   <- simulate_cohort(n = 250, "discovery",   effect_age = 0.40, effect_treatment = 0.55)

replication <- simulate_cohort(n = 180, "replication", effect_age = 0.35, effect_treatment = 0.45)

combined <- bind_rows(discovery, replication)

head(combined)
#> # A tibble: 6 × 7
#>   subject_id  cohort      age sex   treatment     nfl    ab42
#>   <chr>       <chr>     <dbl> <chr> <fct>       <dbl>   <dbl>
#> 1 discovery_1 discovery  76   F     active    2.15    -0.693 
#> 2 discovery_2 discovery  60.5 F     active    1.24    -2.03  
#> 3 discovery_3 discovery  67.9 M     active    0.705    0.0556
#> 4 discovery_4 discovery  70.1 M     control   0.406    0.512 
#> 5 discovery_5 discovery  68.2 F     active    0.00229 -1.86  
#> 6 discovery_6 discovery  64.2 M     active    0.319    0.450
```

## Step 1: pre-flight alignment check

Before pooling, it’s worth confirming that the two cohorts share the
same set of group/term combinations you plan to analyze.
[`check_cohort_alignment()`](https://anaboeriu14.github.io/adRutils/reference/check_cohort_alignment.md)
flags any combinations present in only one cohort.

``` r

check_cohort_alignment(
  discovery, replication,
  by      = c("treatment", "sex"),
  x_label = "discovery",
  y_label = "replication"
)
```

A perfect match is the happy path. In a real analysis, this step often
catches issues like a category coded `"Control"` in one cohort and
`"control"` in the other, or a level present in only one site.

## Step 2: fit models in each cohort

[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
fits a linear model for every (outcome, group) combination. By passing
`group_col = "cohort"`, we get one model per outcome per cohort —
exactly what we need before pooling.

``` r

results <- fit_models_by_group(
  data            = combined,
  outcomes        = c("nfl", "ab42"),
  base_predictors = c("age", "sex", "treatment"),
  group_col       = "cohort",
  groups          = c("discovery", "replication"),
  quiet           = TRUE
)

results %>%
  select(outcome, cohort, model_equation, n_obs)
#> # A tibble: 4 × 4
#>   outcome cohort      model_equation               n_obs
#>   <chr>   <chr>       <chr>                        <int>
#> 1 nfl     discovery   nfl ~ age + sex + treatment    250
#> 2 nfl     replication nfl ~ age + sex + treatment    180
#> 3 ab42    discovery   ab42 ~ age + sex + treatment   250
#> 4 ab42    replication ab42 ~ age + sex + treatment   180
```

The returned tibble has list-columns for the fitted model object
(`model_obj`) and tidy results (`model_res`), plus the data subset used
(`dataf`). For pooling, we only need the tidy coefficients.

## Step 3: extract tidy coefficients

[`extract_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_coefficients.md)
unnests `model_res` and adds confidence intervals computed from the
standard errors. We can filter to a specific term — here, the treatment
effect.

``` r

treatment_effects <- extract_coefficients(
  results,
  term_pattern = "treatmentactive"
)

treatment_effects %>%
  select(outcome, cohort, term, estimate, std.error, conf.low, conf.high, p.value)
#> # A tibble: 4 × 8
#>   outcome cohort      term         estimate std.error conf.low conf.high p.value
#>   <chr>   <chr>       <chr>           <dbl>     <dbl>    <dbl>     <dbl>   <dbl>
#> 1 nfl     discovery   treatmentac…   0.590      0.126    0.342     0.837 5.03e-6
#> 2 nfl     replication treatmentac…   0.388      0.141    0.112     0.664 6.44e-3
#> 3 ab42    discovery   treatmentac…   0.0588     0.137   -0.210     0.327 6.68e-1
#> 4 ab42    replication treatmentac…   0.172      0.155   -0.132     0.476 2.70e-1
```

We now have one row per cohort per outcome — the input shape
[`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)
expects.

## Step 4: REML pooling

[`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)
runs random-effects meta-analysis (REML via
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html))
within each group defined by `group_vars`, appends a pooled row, and
adds a `weight` column showing each cohort’s percent contribution to the
pooled estimate.

``` r

pooled <- add_meta_pooled_results(
  treatment_effects,
  group_vars = c("outcome", "term")
)

pooled %>%
  select(outcome, cohort, estimate, std.error, conf.low, conf.high,
         weight, I2, same_direction, replicated)
#> # A tibble: 6 × 10
#>   outcome cohort      estimate std.error conf.low conf.high weight    I2
#>   <chr>   <fct>          <dbl>     <dbl>    <dbl>     <dbl>  <dbl> <dbl>
#> 1 ab42    discovery     0.0588     0.137  -0.210      0.327   56.2  NA  
#> 2 ab42    replication   0.172      0.155  -0.132      0.476   43.8  NA  
#> 3 ab42    Pooled        0.108      0.103  -0.0930     0.310   NA     0  
#> 4 nfl     discovery     0.590      0.126   0.342      0.837   54.8  NA  
#> 5 nfl     replication   0.388      0.141   0.112      0.664   45.2  NA  
#> 6 nfl     Pooled        0.499      0.100   0.302      0.695   NA    11.7
#> # ℹ 2 more variables: same_direction <lgl>, replicated <lgl>
```

A few things to notice in the output:

- **`weight`** is filled in for cohort rows (REML contribution as a
  percent) and `NA` on the pooled row.
- **`I2`, `Q`, `tau2`** are heterogeneity statistics, attached only to
  the pooled row.
- **`same_direction`** is `TRUE` when the discovery and replication
  estimates have the same sign.
- **`replicated`** is `TRUE` when (a) the signs agree *and* (b) the
  replication estimate has \|β/SE\| \> 1.96.

## Interpreting `replicated`

The `replicated` flag is a useful heuristic but not a formal test. It
captures the common-sense idea that a finding replicates if it points
the same direction in both cohorts and reaches roughly nominal
significance in the second. Two caveats:

1.  It only applies when k = 2. For k ≥ 3 cohorts, sign agreement and
    “the replication estimate” stop being well-defined concepts, so both
    columns are set to `NA`. If you have three or more cohorts, lean on
    the pooled p-value, `I2`, and the cohort-level CIs instead.
2.  The 1.96 threshold treats the replication cohort as a *separate*
    significance test rather than building a joint replication
    statistic. Methods like the Sidak-corrected replication test are
    stricter; this flag is best read as “directionally consistent and
    individually significant in the replication.”

## What about k ≥ 3?

If you have more than two cohorts, the same pipeline works —
[`add_meta_pooled_results()`](https://anaboeriu14.github.io/adRutils/reference/add_meta_pooled_results.md)
will still run REML and report `weight`, `I2`, `Q`, and `tau2` — but
`same_direction` and `replicated` will be `NA` on the pooled rows.
Heterogeneity (`I2`) becomes the more informative signal at that point:
low `I2` with a tight pooled CI is the multi-cohort analog of a clean
replication.

## Recap

The pipeline is:

    check_cohort_alignment()        # pre-flight
      ↓
    fit_models_by_group()           # parallel fits
      ↓
    extract_coefficients()          # tidy + CIs
      ↓
    add_meta_pooled_results()       # REML pool

Each step takes the previous step’s output as-is. If you want to filter
or transform between steps (e.g., drop unstable estimates before
pooling), do it on the tidy frame after
[`extract_coefficients()`](https://anaboeriu14.github.io/adRutils/reference/extract_coefficients.md)
— that’s the most ergonomic point in the pipeline to intervene.
