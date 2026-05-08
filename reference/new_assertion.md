# Create a new validation assertion

Low-level constructor for building custom assertion helpers. Most users
should use the built-in helpers
([`is_flag()`](https://anaboeriu14.github.io/adRutils/reference/is_flag.md),
[`is_string()`](https://anaboeriu14.github.io/adRutils/reference/is_string.md),
etc.) or
[`custom_check()`](https://anaboeriu14.github.io/adRutils/reference/custom_check.md)
for one-off checks. Use `new_assertion()` only when building a
*reusable* assertion that you want to call from multiple functions.

## Usage

``` r
new_assertion(check_fn)
```

## Arguments

- check_fn:

  A function `function(x, nm)` that returns `NULL` on success or a
  cli-flavored error message string on failure. `x` is the value being
  checked; `nm` is the argument name (used in the error message).

## Value

An object of class `"adRutils_assertion"`.

## Examples

``` r
is_probability <- function() {
  new_assertion(function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
      sprintf("{.arg {nm}} must be a single number in [0, 1], not %s",
              format(x))
    }
  })
}
```
