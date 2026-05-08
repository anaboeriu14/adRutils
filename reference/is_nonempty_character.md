# Assert a non-empty character vector

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument is a character vector of length at least 1.

## Usage

``` r
is_nonempty_character()
```

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(vars = is_nonempty_character())
} # }
```
