# Assert one of a fixed set of values

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument matches one of a set of allowed values.

## Usage

``` r
is_one_of(choices)
```

## Arguments

- choices:

  A vector of allowed values.

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(method = is_one_of(c("bonferroni", "holm", "BH")))
} # }
```
