# Data cleaning pipeline

This vignette demonstrates how the data-cleaning functions in adRutils
work together on a realistically messy dataset. The functions are
designed to work together seamlessly: whenever possible, they add new
columns rather than mutating existing ones, making transformations easy
to inspect and reverse. The example builds a pipeline that addresses
missing data, duplicates, outliers, longitudinal coalescing, and
binning—roughly in the order these issues are typically encountered.

``` r

library(adRutils)
library(dplyr)
```

## A messy dataset

We’ll simulate a study dataset with the kinds of problems that actually
show up: duplicate rows from a faulty join, columns that are nearly all
`NA`, outliers from data-entry errors, the same measure collected at
multiple visits, and a continuous variable that needs binning for
downstream analysis.

``` r

set.seed(7)

n <- 100
raw_data <- tibble::tibble(
  subject_id     = c(sprintf("S%03d", 1:n), "S042", "S073"),  # two duplicates
  age            = c(round(rnorm(n, 65, 8), 1), 67, 71),
  bmi            = c(round(rnorm(n, 26, 4), 1), 27, 24),
  weight_kg      = c(round(rnorm(n, 75, 12), 1), 250, 78),    # one outlier
  almost_empty   = c(rep(NA_real_, n - 1), 1.5, NA, NA),
  fully_empty    = rep(NA_real_, n + 2),
  dsst_v1        = round(rnorm(n + 2, 50, 10)),
  dsst_v2        = ifelse(runif(n + 2) > 0.4, round(rnorm(n + 2, 52, 10)), NA),
  visit_year     = sample(c(2020, 2021), n + 2, replace = TRUE)
)

# inject a few NAs in dsst_v1 too
raw_data$dsst_v1[sample(seq_len(nrow(raw_data)), 15)] <- NA

dim(raw_data)
#> [1] 102   9
```

## Step 1: inspect missingness

Always look at NA patterns before you delete anything.
[`summarize_na()`](https://anaboeriu14.github.io/adRutils/reference/summarize_na.md)
gives you a sorted view;
[`drop_high_na_cols()`](https://anaboeriu14.github.io/adRutils/reference/drop_high_na_cols.md)
removes columns above a threshold.

``` r

summarize_na(raw_data)
#> # A tibble: 9 × 3
#>   column       count_na percent_na
#>   <chr>           <dbl>      <dbl>
#> 1 fully_empty       102      100  
#> 2 almost_empty      101       99.0
#> 3 dsst_v2            42       41.2
#> 4 dsst_v1            15       14.7
#> 5 subject_id          0        0  
#> 6 age                 0        0  
#> 7 bmi                 0        0  
#> 8 weight_kg           0        0  
#> 9 visit_year          0        0
```

`fully_empty` is 100% missing and `almost_empty` is close to it. We can
drop both with a threshold of, say, 90%:

``` r

data <- drop_high_na_cols(raw_data, threshold = 90)
names(data)
#> [1] "subject_id" "age"        "bmi"        "weight_kg"  "dsst_v1"   
#> [6] "dsst_v2"    "visit_year"
```

[`drop_high_na_cols()`](https://anaboeriu14.github.io/adRutils/reference/drop_high_na_cols.md)
prints what it removed; if you want the same view without modifying the
data, call `summarize_na(raw_data, threshold = 90)`.

## Step 2: deduplicate

Our synthetic data has two duplicated subject IDs (`S042`, `S073`).
[`remove_duplicates()`](https://anaboeriu14.github.io/adRutils/reference/remove_duplicates.md)
defaults to keeping the row with the fewest NAs per duplicate ID —
usually what you want when one record is more complete than the other.

``` r

data <- remove_duplicates(data, subject_id)
nrow(data)
#> [1] 100
```

The other strategies are `keep = "first"`, `"last"`, and `"none"` (which
removes *all* duplicates including the originals — useful when you
suspect any duplicated ID is a data integrity problem).

## Step 3: combine longitudinal visits

`dsst_v1` and `dsst_v2` are the same measure collected at two visits.
Often you want a single coalesced column with a preferred source, plus a
tracking column showing which visit contributed for each row.

``` r

data <- combine_timepoints(
  data,
  base_names = "dsst",
  timepoints = c("_v1", "_v2"),
  timepoint_labels = c("_v1" = "Visit 1", "_v2" = "Visit 2")
)

data %>%
  select(subject_id, dsst_v1, dsst_v2, dsst_final, dsst_source) %>%
  head()
#> # A tibble: 6 × 5
#>   subject_id dsst_v1 dsst_v2 dsst_final dsst_source
#>   <chr>        <dbl>   <dbl>      <dbl> <chr>      
#> 1 S001            55      NA         55 Visit 1    
#> 2 S002            47      NA         47 Visit 1    
#> 3 S003            51      43         51 Visit 1    
#> 4 S004            46      NA         46 Visit 1    
#> 5 S005            46      43         46 Visit 1    
#> 6 S006            38      57         38 Visit 1
```

`dsst_final` takes the first non-missing value across visits in the
priority order you specified; `dsst_source` records which visit it came
from. The original `dsst_v1` and `dsst_v2` are preserved unchanged.

## Step 4: handle outliers

Our `weight_kg` column has a 250 kg value that’s almost certainly a
data-entry error.
[`replace_outliers_with_na()`](https://anaboeriu14.github.io/adRutils/reference/replace_outliers_with_na.md)
uses the IQR rule (Tukey’s 1.5×IQR by default) to identify and replace
outliers with `NA`.

``` r

summary(data$weight_kg)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   39.30   67.15   74.40   74.58   84.08  108.00

data <- replace_outliers_with_na(
  data,
  var_names  = "weight_kg",
  multiplier = 1.5
)

summary(data$weight_kg)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   48.90   67.20   74.70   74.94   84.25  108.00
```

Two things worth knowing about how this works:

1.  The function is **idempotent** — running it twice produces the same
    result, since outliers become `NA` and `NA` is skipped on subsequent
    passes.
2.  If you have *paired* columns (e.g., `weight` and `log_weight`), the
    `paired_cols` argument nullifies the paired entry whenever the
    source column has an outlier, keeping derived columns consistent
    with their source.

If you only want to inspect the cutoffs without replacing anything,
[`detect_outlier_thresholds()`](https://anaboeriu14.github.io/adRutils/reference/detect_outlier_thresholds.md)
returns the lower and upper bounds:

``` r

detect_outlier_thresholds(data, "weight_kg", multiplier = 1.5)
#>       LB      UB     notes
#> 1 41.625 109.825 weight_kg
```

## Step 5: bin continuous variables

Many downstream analyses want categorical age groups, BMI categories,
etc.
[`bin_and_categorize_variables()`](https://anaboeriu14.github.io/adRutils/reference/bin_and_categorize_variables.md)
handles three transformation types — cutpoints (continuous), categorical
mappings, and custom functions — through a single interface.

``` r

data <- bin_and_categorize_variables(data, groups = list(
  list(
    col       = "age",
    type      = "cutpoints",
    cutpoints = c(60, 75),
    labels    = c("Younger", "Middle", "Older"),
    name      = "age_group"
  ),
  list(
    col       = "bmi",
    type      = "cutpoints",
    cutpoints = c(18.5, 25, 30),
    labels    = c("Underweight", "Normal", "Overweight", "Obese"),
    name      = "bmi_category"
  )
))

data %>%
  count(age_group, bmi_category)
#> # A tibble: 10 × 3
#>    age_group bmi_category     n
#>    <fct>     <fct>        <int>
#>  1 Younger   Normal           8
#>  2 Younger   Overweight      13
#>  3 Younger   Obese            1
#>  4 Middle    Underweight      2
#>  5 Middle    Normal          16
#>  6 Middle    Overweight      31
#>  7 Middle    Obese           16
#>  8 Older     Underweight      1
#>  9 Older     Normal           3
#> 10 Older     Overweight       8
```

A note on bin semantics: cutpoints are interpreted with
[`cut()`](https://rdrr.io/r/base/cut.html)’s left-open right-closed
rule, except the leftmost interval is closed on both ends (matching
`include.lowest = TRUE`). So with cutpoints `c(18.5, 25, 30)`, a BMI of
exactly 25 falls into `Normal`, not `Overweight`.

## Putting it together

The whole pipeline reads cleanly as a single chain:

``` r

clean_data <- raw_data %>%
  drop_high_na_cols(threshold = 90) %>%
  remove_duplicates(subject_id) %>%
  combine_timepoints(
    base_names = "dsst",
    timepoints = c("_v1", "_v2")
  ) %>%
  replace_outliers_with_na(var_names = "weight_kg") %>%
  bin_and_categorize_variables(groups = list(
    list(col = "age", type = "cutpoints", cutpoints = c(60, 75)),
    list(col = "bmi", type = "cutpoints", cutpoints = c(18.5, 25, 30))
  ))
```

## What to do next

After cleaning, the typical next steps are:

- Inspect the cleaned dataset with
  [`summarize_na()`](https://anaboeriu14.github.io/adRutils/reference/summarize_na.md)
  again to confirm nothing unexpected is missing.
- Convert pattern-named columns to factors with
  [`convert_columns_to_factors()`](https://anaboeriu14.github.io/adRutils/reference/convert_columns_to_factors.md)
  if you have many of them.
- Pull related-but-named-differently columns together with
  [`coalesce_variables()`](https://anaboeriu14.github.io/adRutils/reference/coalesce_variables.md)
  if your dataset uses inconsistent naming across sources.
- Move on to modeling with
  [`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
  — see the meta-analysis vignette for a worked example.
