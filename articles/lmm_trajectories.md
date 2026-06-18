# Trajectory Classification from Mixed-Effects Models

[`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
extracts random slopes from a fitted `lme4` model, z-scores them, and
assigns each subject to a trajectory group: low / mid / high.

This vignette walks through the typical use case — taking longitudinal
data, fitting a mixed-effects model with random slopes, and using the
resulting groups as a downstream predictor — and compares the four
available classification methods on the same data.

``` r

library(adRutils)
library(dplyr)
library(lme4)
```

## Why classify trajectories?

Many longitudinal studies want to ask: *who is declining faster than
expected?* The natural quantity is the random slope from a mixed-effects
model — each subject’s deviation from the overall trend. But a
continuous slope estimate isn’t directly usable as a group label, and it
isn’t directly comparable across studies. Discretizing into low / mid /
high gives you a categorical predictor you can drop into downstream
models, plot stratified by group, and report in a table.

This is a conceptual simplification and you should think about whether
it’s appropriate for your analysis — the slope is a noisy estimate,
especially for subjects with few visits, and binning loses information.
But when the goal is “find the decliners and describe what they look
like,” it’s a reasonable first move.

## Simulate longitudinal data

We’ll simulate 200 subjects with 3–6 visits each, a shared linear trend,
and subject-specific random intercepts and slopes.

``` r

set.seed(2024)

n_subjects <- 200

subjects <- tibble::tibble(
  subject_id      = sprintf("sub_%03d", seq_len(n_subjects)),
  baseline_age    = round(rnorm(n_subjects, 70, 6), 1),
  random_intercept = rnorm(n_subjects, 0, 1),
  random_slope    = rnorm(n_subjects, -0.4, 0.3)   # average decline of -0.4 / yr
)

long_df <- subjects %>%
  rowwise() %>%
  mutate(
    n_visits = sample(3:6, 1),
    visits   = list(sort(round(c(0, runif(n_visits - 1, 1, 5)), 2)))
  ) %>%
  tidyr::unnest(visits) %>%
  rename(time = visits) %>%
  mutate(
    score = 50 +
      random_intercept +
      (-0.4 + random_slope) * time +
      rnorm(n(), 0, 1)
  ) %>%
  select(subject_id, baseline_age, time, score) %>%
  ungroup()

head(long_df, 10)
#> # A tibble: 10 × 4
#>    subject_id baseline_age  time score
#>    <chr>             <dbl> <dbl> <dbl>
#>  1 sub_001            75.9  0     48.9
#>  2 sub_001            75.9  1.67  48.6
#>  3 sub_001            75.9  2.11  49.0
#>  4 sub_001            75.9  4.51  50.6
#>  5 sub_002            72.8  0     49.2
#>  6 sub_002            72.8  2.73  50.0
#>  7 sub_002            72.8  3.5   48.4
#>  8 sub_002            72.8  4.13  46.2
#>  9 sub_003            69.4  0     50.3
#> 10 sub_003            69.4  1.65  48.1
```

## Fit a mixed-effects model

Standard random-intercepts-and-slopes specification: each subject gets
their own intercept and their own slope on `time`.

``` r

fit <- lmer(score ~ time + (1 + time | subject_id), data = long_df)
summary(fit)$coefficients
#>              Estimate Std. Error   t value
#> (Intercept) 50.133113 0.08417970 595.54874
#> time        -0.819343 0.03298606 -24.83907
```

The fixed effect on `time` recovers the average decline (~-0.4). What we
want for classification is the per-subject *deviation* from that
average, which lives in the random effects.

## Classify with all four methods

[`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
accepts any subset of `"1sd"`, `"1.5sd"`, `"tertile"`, and `"quartile"`.
Asking for all four lets us compare them side by side.

``` r

trajectories <- classify_trajectory_groups(
  fit,
  id_col     = "subject_id",
  slope_term = "time",
  methods    = c("1sd", "1.5sd", "tertile", "quartile"),
  labels     = list(low = "Decliner", mid = "Stable", high = "Improver"),
  prefix     = "traj"
)

head(trajectories)
#> # A tibble: 6 × 8
#>   subject_id random_intercept random_slope slope_z traj_1sd traj_1.5sd
#>   <chr>                 <dbl>        <dbl>   <dbl> <fct>    <fct>     
#> 1 sub_001             -0.231         0.592   2.22  Improver Improver  
#> 2 sub_002              0.0402        0.163   0.610 Stable   Stable    
#> 3 sub_003             -0.190        -0.161  -0.603 Stable   Stable    
#> 4 sub_004              0.0571       -0.348  -1.30  Decliner Stable    
#> 5 sub_005              0.681         0.350   1.31  Improver Stable    
#> 6 sub_006             -0.829        -0.427  -1.60  Decliner Decliner  
#> # ℹ 2 more variables: traj_tertile <fct>, traj_quartile <fct>
```

The output has one row per subject, with the random intercept (when the
model has one), the raw random slope, the z-scored slope, and one factor
column per requested method.

## How the methods differ

The four methods answer subtly different questions:

- **SD-based (`1sd`, `1.5sd`)** thresholds the *z-scored slope*.
  Subjects more than 1 (or 1.5) SDs from the group mean are flagged. The
  cutoffs are anchored to the spread of the data — fewer subjects fall
  in the extreme groups when the distribution is tight.
- **Quantile-based (`tertile`, `quartile`)** thresholds the *raw slope*
  and guarantees a fixed share in each bucket: ~33% / 34% / 33% for
  tertiles, ~25% / 50% / 25% for quartiles.

A quick comparison of group sizes:

``` r

trajectories %>%
  count(traj_1sd) %>%
  rename(group = traj_1sd, n_1sd = n) %>%
  full_join(
    trajectories %>% count(traj_1.5sd) %>% rename(group = traj_1.5sd, n_1.5sd = n),
    by = "group"
  ) %>%
  full_join(
    trajectories %>% count(traj_tertile) %>% rename(group = traj_tertile, n_tertile = n),
    by = "group"
  ) %>%
  full_join(
    trajectories %>% count(traj_quartile) %>% rename(group = traj_quartile, n_quartile = n),
    by = "group"
  )
#> # A tibble: 3 × 5
#>   group    n_1sd n_1.5sd n_tertile n_quartile
#>   <fct>    <int>   <int>     <int>      <int>
#> 1 Stable     142     174        66        100
#> 2 Decliner    31      15        67         50
#> 3 Improver    27      11        67         50
```

`1sd` flags ~32% of subjects as extreme (16% each tail); `1.5sd` is much
stricter. Tertiles guarantee roughly equal-sized groups regardless of
the slope distribution.

**Which to pick?** It depends on the downstream question:

- If you want roughly balanced groups for stratified plotting or
  cross-tabulation: tertiles or quartiles.
- If you want to flag *unusual* trajectories where “unusual” is defined
  relative to the cohort’s spread: `1sd` or `1.5sd`.
- If you’re not sure, run two methods and check that your downstream
  conclusions don’t hinge on the choice. Stable conclusions are more
  trustworthy than ones that flip between tertile and 1.5sd.

## Using groups as downstream predictors

The whole point of classifying is usually to feed the groups into
something else. Here we’ll merge the trajectory labels back to a
subject-level table and use
[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
to look at how baseline age varies by trajectory.

``` r

subject_level <- long_df %>%
  group_by(subject_id, baseline_age) %>%
  summarise(.groups = "drop") %>%
  left_join(trajectories, by = "subject_id")

age_by_traj <- fit_models_by_group(
  data            = subject_level,
  outcomes        = "baseline_age",
  base_predictors = "traj_tertile",
  quiet           = TRUE
)

extract_coefficients(age_by_traj)
#> # A tibble: 3 × 15
#>   outcome      group model predictors   model_equation  term  estimate std.error
#>   <chr>        <chr> <chr> <chr>        <chr>           <chr>    <dbl>     <dbl>
#> 1 baseline_age All   main  traj_tertile baseline_age ~… (Int…   69.1       0.753
#> 2 baseline_age All   main  traj_tertile baseline_age ~… traj…    2.17      1.06 
#> 3 baseline_age All   main  traj_tertile baseline_age ~… traj…    0.979     1.06 
#> # ℹ 7 more variables: statistic <dbl>, p.value <dbl>, model_error <chr>,
#> #   n_obs <int>, conf.low <dbl>, conf.high <dbl>, sig <lgl>
```

The `traj_tertile` coefficients are contrasts against the reference
level (`Stable` by default —
[`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
puts `mid` first because most analyses contrast extremes against
typical).

## A warning sign

If `lme4::isSingular(fit)` is `TRUE`, or if the model summary reports a
near-zero variance on the slope term, the random slopes are unstable and
so are any classifications based on them.
[`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
will warn in this case but still return a result — the warning is your
cue to revisit the model, not to silently use the output.

``` r

isSingular(fit)
#> [1] FALSE
```

If you do see singularity, common fixes include simplifying the
random-effects structure, increasing the number of visits per subject
(if you have control over data collection), or accepting that trajectory
classification isn’t well-supported by your data.

## Recap

The full pipeline is:

    fit lmer with (1 + time | subject)
      ↓
    classify_trajectory_groups()
      ↓
    join groups back to subject-level data
      ↓
    fit_models_by_group() / plotting / tables

Most of the complexity is in choosing the classification method and
reading the random-effects diagnostics —
[`classify_trajectory_groups()`](https://anaboeriu14.github.io/adRutils/reference/classify_trajectory_groups.md)
itself just bundles the conventional steps (extract, z-score, threshold)
into one call.
