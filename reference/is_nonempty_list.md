# Assert a non-empty list

Use inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md)
to check that an argument is a list of length at least 1.

## Usage

``` r
is_nonempty_list()
```

## Value

An assertion object consumed by
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(groups = is_nonempty_list())
} # }
```
