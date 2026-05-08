# Bin and categorize variables into groups

Transforms variables into categorical groups using cutpoints
(continuous), value mappings (categorical), or custom functions.

## Usage

``` r
bin_and_categorize_variables(
  dataf,
  groups,
  filter_missing = FALSE,
  quiet = FALSE
)
```

## Arguments

- dataf:

  A data frame.

- groups:

  A list of group specifications. Each element is itself a list with
  these fields:

  - `col` (required): Name of the column in `dataf` to transform.

  - `type` (required): One of `"cutpoints"`, `"categorical"`, or
    `"custom"`.

  - `name` (optional): Output column name. Defaults to `<col>_group`.

  - `cutpoints` (required for `type = "cutpoints"`): Numeric vector of
    internal break points, in increasing order. Intervals are left-open,
    right-closed, except the leftmost which is closed on both ends
    (matching [`base::cut()`](https://rdrr.io/r/base/cut.html) with
    `include.lowest = TRUE`).

  - `labels` (optional, for `type = "cutpoints"`): Character vector of
    length `length(cutpoints) + 1`. Defaults to interval notation
    derived from the cutpoints.

  - `values` (required for `type = "categorical"`): Named vector mapping
    original values to new labels. All mappings are applied
    simultaneously, so chained remappings will not occur.

  - `custom_fn` (required for `type = "custom"`): A function taking the
    column vector and returning a vector of the same length.

- filter_missing:

  If `TRUE`, drop rows with `NA` in any newly created group column.
  Default `FALSE`.

- quiet:

  If `TRUE`, suppress informational messages. Default `FALSE`.

## Value

`dataf` with one new column per element of `groups`.

## Examples

``` r
df <- data.frame(
  age      = c(45, 67, 72, 81, 55),
  sex_code = c(1, 2, 1, 2, 1),
  bmi      = c(22.4, 28.1, 31.5, 24.8, 35.2)
)

bin_and_categorize_variables(df, groups = list(
  list(col = "age",      type = "cutpoints",   cutpoints = c(60, 75)),
  list(col = "sex_code", type = "categorical",
       values = c("1" = "Male", "2" = "Female"),
       name   = "sex"),
  list(col = "bmi",      type = "cutpoints",
       cutpoints = c(18.5, 25, 30),
       labels    = c("Underweight", "Normal", "Overweight", "Obese"))
))
#> ✔ Created 3 grouped variables
#>   age sex_code  bmi age_group    sex  bmi_group
#> 1  45        1 22.4      <=60   Male     Normal
#> 2  67        2 28.1   (60,75] Female Overweight
#> 3  72        1 31.5   (60,75]   Male      Obese
#> 4  81        2 24.8       >75 Female     Normal
#> 5  55        1 35.2      <=60   Male      Obese
```
