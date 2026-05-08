# Assert a single logical value (`TRUE` or `FALSE`, not `NA`)

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument is a single non-missing logical value.

## Usage

``` r
is_flag()
```

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(quiet = is_flag())
} # }
```
