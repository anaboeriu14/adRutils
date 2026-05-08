# Build a lazily-evaluated custom validation check

Captures `condition` as an unevaluated expression so it can be safely
evaluated inside
[`validate_args()`](https://anaboeriu14.github.io/adRutils/reference/validate_args.md),
even if it references values that might be `NULL` or fail to evaluate.
The expression is evaluated in the calling function's environment by
default.

## Usage

``` r
custom_check(condition, message)
```

## Arguments

- condition:

  An unquoted logical expression (e.g. `nrow(data) > 0`). If evaluation
  throws an error, the check fails with a message referring to the
  original error.

- message:

  A cli-flavored error message to report when the condition is `FALSE`.
  Use `{.arg name}`, `{.val val}`, etc. as in
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

## Value

A list with a quoted `condition` and a `message` string, of class
`"adRutils_custom_check"`.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_args(
  x = is_string(),
  custom_checks = list(
    custom_check(nrow(data) > 0, "{.arg data} must have rows")
  )
)
} # }
```
