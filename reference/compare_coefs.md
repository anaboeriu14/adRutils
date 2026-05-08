# Compare two coefficients via a z-score test

Tests whether two effect estimates differ, given their standard errors,
under the assumption of independent estimates. Implements the z-score
approach described in
[doi:10.1038/s41380-019-0596-9](https://doi.org/10.1038/s41380-019-0596-9)
.

## Usage

``` r
compare_coefs(b1, b2, se1, se2)
```

## Arguments

- b1, b2:

  Numeric scalars: the two coefficient estimates.

- se1, se2:

  Numeric scalars: standard errors of the two estimates. Must be
  positive.

## Value

A named list with:

- `z_statistic` — the z-score for the difference.

- `p_value` — two-tailed p-value.

- `diff` — `b1 - b2`.

- `se_diff` — \\\sqrt{se1^2 + se2^2}\\.

## Examples

``` r
compare_coefs(b1 = 0.50, b2 = 0.30, se1 = 0.10, se2 = 0.12)
#> $z_statistic
#> [1] 1.280369
#> 
#> $p_value
#> [1] 0.2004155
#> 
#> $diff
#> [1] 0.2
#> 
#> $se_diff
#> [1] 0.156205
#> 
```
