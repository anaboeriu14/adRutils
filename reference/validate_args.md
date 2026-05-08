# Validate function inputs with consistent error messages

Runs a set of named assertions against function arguments and reports
all failures together. Designed to replace ad-hoc validation scattered
through individual functions with a single, uniform interface.

## Usage

``` r
validate_args(
  ...,
  data = NULL,
  columns = NULL,
  numeric_columns = NULL,
  custom_checks = list(),
  context = NULL,
  .envir = rlang::caller_env()
)
```

## Arguments

- ...:

  Named assertions, e.g. `quiet = is_flag()`. Each name must refer to an
  argument of the calling function.

- data:

  Optional data frame. When supplied, enables the `columns =` and
  `numeric_columns =` shorthand for column existence/type checks.

- columns:

  Character vector of columns required to exist in `data`.

- numeric_columns:

  Character vector of columns required to exist in `data` AND be
  numeric.

- custom_checks:

  A list of
  [`custom_check()`](https://anaboeriu14.github.io/adRutils/reference/custom_check.md)
  results (or equivalent `list(condition, message)` pairs with quoted
  conditions) for domain-specific checks that don't fit the standard
  shapes.

- context:

  Calling function name. If `NULL`, inferred from the call stack.

- .envir:

  The environment in which to look up argument values. Defaults to the
  calling environment, which is correct for the typical use of calling
  `validate_args()` directly inside a function. Pass an explicit
  environment when wrapping `validate_args()` in a helper.

## Value

Invisibly `TRUE` if all checks pass; aborts with a consolidated error
message otherwise.

## Details

Custom checks accept *unevaluated* expressions, so it is safe to
reference values that may be `NULL` or otherwise problematic — the
expression is only evaluated inside `validate_args()`, where errors are
caught and reported as validation failures rather than propagating up to
the caller. Use
[`custom_check()`](https://anaboeriu14.github.io/adRutils/reference/custom_check.md)
(or a `list(condition = ..., message = ...)` pair, where `condition` is
wrapped in
[`rlang::quo()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
or the call is deferred):

    validate_args(
      x = is_string(),
      custom_checks = list(
        custom_check(nrow(data) > 0, "{.arg data} must have at least one row")
      )
    )

## Examples

``` r
if (FALSE) { # \dontrun{
my_function <- function(dataf, vars, threshold = 0.5, quiet = FALSE) {
  validate_args(
    data            = dataf,
    columns         = vars,
    numeric_columns = vars,
    vars            = is_nonempty_character(),
    threshold       = is_number(min = 0, max = 1),
    quiet           = is_flag(),
    custom_checks   = list(
      custom_check(nrow(dataf) > 0, "{.arg dataf} must have rows")
    )
  )
  # ... function body ...
}
} # }
```
