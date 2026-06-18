# Log-Transformation and Z-Scores

## Overview

Many analyses need numeric measures put on a common, comparable scale
before modeling. The usual sequence is **handle outliers → transform
skewed variables → standardize**, and adRutils provides a function for
each step, all of which add new columns rather than overwriting
originals so every stage is easy to inspect and reverse.

This pattern applies to any set of skewed, differently-scaled numeric
measures — assay panels, lab chemistries, gene-expression values,
questionnaire subscales, and so on. We use a small set of plasma
biomarkers as the running example here because they exercise every step
(right-skewed distributions on very different numeric scales), but
nothing in the workflow is specific to biomarkers. The result is a set
of standardized variables (mean 0, SD 1) suitable for comparison or
combining; the outlier and log steps are preparation that makes that
final standardization more robust.

The [Data cleaning
pipeline](https://anaboeriu14.github.io/adRutils/articles/data_cleaning.md)
vignette covers the upstream steps — missingness, duplicates, binning.
This one picks up at the transform-and-standardize stage.

``` r

library(adRutils)
library(dplyr)
```

## An example dataset

Three measures on very different scales: `marker_a` in the hundreds,
`marker_b` and `marker_c` both small and right-skewed.

``` r

set.seed(7)
n <- 60

measures <- tibble::tibble(
  subject_id = sprintf("S%03d", 1:n),
  ancestry   = sample(c("EUR", "AFR"), n, replace = TRUE),
  marker_a   = round(rnorm(n, 900, 150), 1),
  marker_b   = round(rlnorm(n, log(2),  0.5), 2),
  marker_c   = round(rlnorm(n, log(15), 0.6), 1)
)

# inject one implausible value to demonstrate outlier handling
measures$marker_c[1] <- 500

head(measures)
#> # A tibble: 6 × 5
#>   subject_id ancestry marker_a marker_b marker_c
#>   <chr>      <chr>       <dbl>    <dbl>    <dbl>
#> 1 S001       AFR          769.     4.53    500  
#> 2 S002       EUR         1008.     1.45     29  
#> 3 S003       EUR          917.     2.73      8.7
#> 4 S004       AFR          888.     2.25     13.2
#> 5 S005       EUR          837.     3.05     22.5
#> 6 S006       AFR          816.     1.5       9.3
```

## Step 1: Handle outliers

[`replace_outliers_with_na()`](https://anaboeriu14.github.io/adRutils/reference/replace_outliers_with_na.md)
flags values outside the Tukey IQR fence and sets them to `NA`. Apply it
to all three measures at once.

``` r

measures <- replace_outliers_with_na(
  measures,
  var_names          = c("marker_a", "marker_b", "marker_c"),
  multiplier         = 1.5,
  remove_all_na_rows = FALSE
)
#> ✔ Replaced 5 outliers with NA across 3 variables
```

To inspect the cutoffs without altering the data,
[`detect_outlier_thresholds()`](https://anaboeriu14.github.io/adRutils/reference/detect_outlier_thresholds.md)
returns the bounds for a single column:

``` r

detect_outlier_thresholds(measures, "marker_c", multiplier = 1.5)
#>        LB      UB    notes
#> 1 -7.9375 41.1625 marker_c
```

## Step 2: Transform skewed variables

[`transform_log10()`](https://anaboeriu14.github.io/adRutils/reference/transform_log10.md)
adds `log10_<var>` columns and leaves the originals in place. It
requires strictly positive input, which suits concentration-style
measures.

``` r

measures <- transform_log10(
  measures,
  vars = c("marker_a", "marker_b", "marker_c")
)

names(measures)
#> [1] "subject_id"     "ancestry"       "marker_a"       "marker_b"      
#> [5] "marker_c"       "log10_marker_a" "log10_marker_b" "log10_marker_c"
```

## Step 3: Standardize

Standardizing the log-scaled measures to z-scores (mean 0, SD 1) puts
them on a common scale so they can be compared or combined. We
standardize the `log10_` columns from Step 2.

``` r

log_vars <- c("log10_marker_a", "log10_marker_b", "log10_marker_c")

measures <- compute_zscores(
  measures,
  vars   = log_vars,
  prefix = "z_"
)
#> ℹ Standardizing 3 variables
#> ✔ Standardization complete (3 variables processed)

measures %>%
  select(subject_id, dplyr::starts_with("z_")) %>%
  head()
#> # A tibble: 6 × 4
#>   subject_id z_log10_marker_a z_log10_marker_b z_log10_marker_c
#>   <chr>                 <dbl>            <dbl>            <dbl>
#> 1 S001               -1.04              1.67             NA    
#> 2 S002                0.920            -0.931             1.14 
#> 3 S003                0.231             0.513            -0.970
#> 4 S004                0.00181           0.0718           -0.238
#> 5 S005               -0.431             0.766             0.698
#> 6 S006               -0.617            -0.853            -0.853
```

When distributions differ systematically across a grouping variable,
standardizing within strata keeps each group internally comparable. Pass
`group_vars`:

``` r

measures <- compute_zscores(
  measures,
  vars       = log_vars,
  prefix     = "z_within_ancestry_",
  group_vars = "ancestry"
)
#> ℹ Standardizing 3 variables by 1 grouping variable
#> ✔ Standardization complete (3 variables processed)
```

## The pipeline together

``` r

standardized <- raw_measures %>%
  replace_outliers_with_na(
    var_names          = c("marker_a", "marker_b", "marker_c"),
    remove_all_na_rows = FALSE
  ) %>%
  transform_log10(vars = c("marker_a", "marker_b", "marker_c")) %>%
  compute_zscores(
    vars   = c("log10_marker_a", "log10_marker_b", "log10_marker_c"),
    prefix = "z_"
  )
```

## What to do next

The standardized `z_` columns are ready for modeling — for example
fitting models across groups with
[`fit_models_by_group()`](https://anaboeriu14.github.io/adRutils/reference/fit_models_by_group.md)
([REML
Meta-analysis](https://anaboeriu14.github.io/adRutils/articles/reml_meta_analysis.md)),
or building composite indices from the standardized values.

## Session information

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyr_1.2.1    adRutils_2.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] jsonlite_2.0.0    compiler_4.6.0    tidyselect_1.2.1  stringr_1.6.0    
#>  [5] snakecase_0.11.1  tidyr_1.3.2       jquerylib_0.1.4   systemfonts_1.3.2
#>  [9] textshaping_1.0.5 yaml_2.3.12       fastmap_1.2.0     here_1.0.2       
#> [13] readr_2.2.0       R6_2.6.1          generics_0.1.4    knitr_1.51       
#> [17] backports_1.5.1   tibble_3.3.1      janitor_2.2.1     desc_1.4.3       
#> [21] rprojroot_2.1.1   lubridate_1.9.5   tzdb_0.5.0        bslib_0.11.0     
#> [25] pillar_1.11.1     rlang_1.2.0       utf8_1.2.6        cachem_1.1.0     
#> [29] broom_1.0.13      stringi_1.8.7     xfun_0.58         fs_2.1.0         
#> [33] sass_0.4.10       otel_0.2.0        timechange_0.4.0  cli_3.6.6        
#> [37] withr_3.0.2       pkgdown_2.2.0     magrittr_2.0.5    digest_0.6.39    
#> [41] hms_1.1.4         lifecycle_1.0.5   vctrs_0.7.3       evaluate_1.0.5   
#> [45] glue_1.8.1        ragg_1.5.2        rmarkdown_2.31    purrr_1.2.2      
#> [49] tools_4.6.0       pkgconfig_2.0.3   htmltools_0.5.9
```
