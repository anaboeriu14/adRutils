# Assert a single non-empty string

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument is a single non-missing, non-empty character
value.

## Usage

``` r
is_string()
```

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(group_var = is_string())
} # }
```
