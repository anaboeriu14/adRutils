# Coalesce related variables into a single column

Combines groups of columns into one column per group by taking the first
non-missing value across them (using
[`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)).
Groups can be specified manually via `var_groups` or discovered
automatically via a regex `pattern_extract` that identifies the varying
portion of column names.

## Usage

``` r
coalesce_variables(
  dataf,
  pattern_extract = NULL,
  var_groups = NULL,
  prefix = "",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame containing the variables to coalesce.

- pattern_extract:

  Regex matching the varying segment of column names. Columns are
  grouped by the *non-matching* portion of their name. Mutually
  exclusive with `var_groups`.

- var_groups:

  Named list of column groups. Each name becomes the new coalesced
  column name. Mutually exclusive with `pattern_extract`.

- prefix:

  String to prepend to new column names. Default `""`.

- overwrite:

  If `TRUE`, replace existing columns; if `FALSE` (default), skip them
  with a warning.

- quiet:

  If `TRUE`, suppress messages. Default `FALSE`.

## Value

`dataf` with coalesced columns added.

## Examples

``` r
df <- data.frame(
  bmi_v1 = c(22, NA, 28),
  bmi_v2 = c(NA, 25, 27),
  age_v1 = c(45, NA, 70),
  age_v2 = c(NA, 60, NA)
)

# Pattern-based: strip "_v[12]" from each name and group by the rest
coalesce_variables(df, pattern_extract = "_v[12]")
#> ✔ Created 2 coalesced columns
#>   bmi_v1 bmi_v2 age_v1 age_v2 bmi age
#> 1     22     NA     45     NA  22  45
#> 2     NA     25     NA     60  25  60
#> 3     28     27     70     NA  28  70

# Manual grouping
coalesce_variables(df, var_groups = list(
  bmi = c("bmi_v1", "bmi_v2"),
  age = c("age_v1", "age_v2")
))
#> ✔ Created 2 coalesced columns
#>   bmi_v1 bmi_v2 age_v1 age_v2 bmi age
#> 1     22     NA     45     NA  22  45
#> 2     NA     25     NA     60  25  60
#> 3     28     27     70     NA  28  70
```
