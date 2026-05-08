# Assert a single numeric value, optionally bounded

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument is a single non-missing numeric value,
optionally requiring it to be positive and/or fall within a range.

## Usage

``` r
is_number(min = -Inf, max = Inf, positive = FALSE)
```

## Arguments

- min, max:

  Inclusive bounds. Use `-Inf` / `Inf` for unbounded.

- positive:

  If `TRUE`, also require `x > 0`.

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(
  alpha = is_number(min = 0, max = 1),
  se    = is_number(positive = TRUE)
)
} # }
```
