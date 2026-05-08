#' Validate function inputs with consistent error messages
#'
#' Runs a set of named assertions against function arguments and reports
#' all failures together. Designed to replace ad-hoc validation scattered
#' through individual functions with a single, uniform interface.
#'
#' @details
#' Custom checks accept *unevaluated* expressions, so it is safe to
#' reference values that may be `NULL` or otherwise problematic --- the
#' expression is only evaluated inside `validate_args()`, where errors are
#' caught and reported as validation failures rather than propagating up
#' to the caller. Use [custom_check()] (or a `list(condition = ..., message = ...)`
#' pair, where `condition` is wrapped in [rlang::quo()] or the call is
#' deferred):
#'
#' ```r
#' validate_args(
#'   x = is_string(),
#'   custom_checks = list(
#'     custom_check(nrow(data) > 0, "{.arg data} must have at least one row")
#'   )
#' )
#' ```
#'
#' @param ... Named assertions, e.g. `quiet = is_flag()`. Each name must
#'   refer to an argument of the calling function.
#' @param data Optional data frame. When supplied, enables the `columns =`
#'   and `numeric_columns =` shorthand for column existence/type checks.
#' @param columns Character vector of columns required to exist in `data`.
#' @param numeric_columns Character vector of columns required to exist in
#'   `data` AND be numeric.
#' @param custom_checks A list of [custom_check()] results (or equivalent
#'   `list(condition, message)` pairs with quoted conditions) for
#'   domain-specific checks that don't fit the standard shapes.
#' @param context Calling function name. If `NULL`, inferred from the call
#'   stack.
#' @param .envir The environment in which to look up argument values.
#'   Defaults to the calling environment, which is correct for the typical
#'   use of calling `validate_args()` directly inside a function. Pass an
#'   explicit environment when wrapping `validate_args()` in a helper.
#'
#' @return Invisibly `TRUE` if all checks pass; aborts with a consolidated
#'   error message otherwise.
#'
#' @examples
#' \dontrun{
#' my_function <- function(dataf, vars, threshold = 0.5, quiet = FALSE) {
#'   validate_args(
#'     data            = dataf,
#'     columns         = vars,
#'     numeric_columns = vars,
#'     vars            = is_nonempty_character(),
#'     threshold       = is_number(min = 0, max = 1),
#'     quiet           = is_flag(),
#'     custom_checks   = list(
#'       custom_check(nrow(dataf) > 0, "{.arg dataf} must have rows")
#'     )
#'   )
#'   # ... function body ...
#' }
#' }
#'
#' @export
validate_args <- function(...,
                          data            = NULL,
                          columns         = NULL,
                          numeric_columns = NULL,
                          custom_checks   = list(),
                          context         = NULL,
                          .envir          = rlang::caller_env()) {

  context <- context %||% .infer_context()
  errors  <- character()

  # --- arg-shape assertions via ... ---
  dots      <- rlang::enquos(..., .ignore_empty = "all")
  dot_names <- names(dots)

  for (i in seq_along(dots)) {
    nm <- dot_names[i]

    if (is.null(nm) || !nzchar(nm)) {
      cli::cli_abort(
        "In {.fn {context}}: assertions passed via {.code ...} must be named."
      )
    }

    assertion <- tryCatch(
      rlang::eval_tidy(dots[[i]]),
      error = function(e) {
        cli::cli_abort(c(
          "In {.fn {context}}: failed to construct assertion for {.arg {nm}}.",
          "x" = conditionMessage(e)
        ))
      }
    )

    if (!inherits(assertion, "adRutils_assertion")) {
      cli::cli_abort(c(
        "In {.fn {context}}: {.arg {nm}} must be an assertion helper, not a bare value.",
        "i" = "Did you mean {.code {nm} = is_flag()} or similar?",
        "i" = "Available helpers: {.fn is_flag}, {.fn is_string}, {.fn is_nonempty_character}, {.fn is_number}, {.fn is_one_of}, {.fn is_nonempty_list}."
      ))
    }

    value <- .lookup_caller_value(nm, .envir, context)
    err   <- assertion$check(value, nm)
    if (!is.null(err)) errors <- c(errors, err)
  }

  # --- data frame check ---
  if (!is.null(data) && !is.data.frame(data)) {
    errors <- c(errors, "Input {.arg data} must be a data frame")
  }

  # --- column existence ---
  if (!is.null(columns) && is.data.frame(data)) {
    missing_cols <- setdiff(columns, names(data))
    if (length(missing_cols) > 0L) {
      errors <- c(errors,
                  "Column{?s} not found in data: {.val {missing_cols}}"
      )
    }
  }

  # --- numeric columns ---
  if (!is.null(numeric_columns) && is.data.frame(data)) {
    missing_num <- setdiff(numeric_columns, names(data))
    present_num <- intersect(numeric_columns, names(data))
    non_numeric <- present_num[!vapply(
      present_num, function(col) is.numeric(data[[col]]), logical(1)
    )]

    if (length(missing_num) > 0L) {
      errors <- c(errors,
                  "Numeric-check column{?s} not found in data: {.val {missing_num}}"
      )
    }
    if (length(non_numeric) > 0L) {
      errors <- c(errors,
                  "Column{?s} not numeric: {.val {non_numeric}}"
      )
    }
  }

  # --- custom checks (lazily evaluated) ---
  for (i in seq_along(custom_checks)) {
    check <- custom_checks[[i]]
    err   <- .eval_custom_check(check, i, .envir)
    if (!is.null(err)) errors <- c(errors, err)
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "In {.fn {context}}: {length(errors)} validation issue{?s} found.",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  invisible(TRUE)
}


#' Build a lazily-evaluated custom validation check
#'
#' Captures `condition` as an unevaluated expression so it can be safely
#' evaluated inside [validate_args()], even if it references values that
#' might be `NULL` or fail to evaluate. The expression is evaluated in the
#' calling function's environment by default.
#'
#' @param condition An unquoted logical expression (e.g. `nrow(data) > 0`).
#'   If evaluation throws an error, the check fails with a message
#'   referring to the original error.
#' @param message A cli-flavored error message to report when the
#'   condition is `FALSE`. Use `{.arg name}`, `{.val val}`, etc. as in
#'   [cli::cli_abort()].
#'
#' @return A list with a quoted `condition` and a `message` string, of
#'   class `"adRutils_custom_check"`.
#'
#' @examples
#' \dontrun{
#' validate_args(
#'   x = is_string(),
#'   custom_checks = list(
#'     custom_check(nrow(data) > 0, "{.arg data} must have rows")
#'   )
#' )
#' }
#'
#' @export
custom_check <- function(condition, message) {
  if (!is.character(message) || length(message) != 1L) {
    cli::cli_abort("{.arg message} must be a single string")
  }
  structure(
    list(
      condition = rlang::enquo(condition),
      message   = message
    ),
    class = "adRutils_custom_check"
  )
}


# --- assertion constructors -------------------------------------------------
# Each public assertion helper returns an object of class
# "adRutils_assertion" with a $check method that takes (value, arg_name)
# and returns NULL on success or an error message string.

#' Create a new validation assertion
#'
#' Low-level constructor for building custom assertion helpers. Most users
#' should use the built-in helpers ([is_flag()], [is_string()], etc.) or
#' [custom_check()] for one-off checks. Use `new_assertion()` only when
#' building a *reusable* assertion that you want to call from multiple
#' functions.
#'
#' @param check_fn A function `function(x, nm)` that returns `NULL` on
#'   success or a cli-flavored error message string on failure. `x` is
#'   the value being checked; `nm` is the argument name (used in the
#'   error message).
#'
#' @return An object of class `"adRutils_assertion"`.
#'
#' @examples
#' is_probability <- function() {
#'   new_assertion(function(x, nm) {
#'     if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
#'       sprintf("{.arg {nm}} must be a single number in [0, 1], not %s",
#'               format(x))
#'     }
#'   })
#' }
#'
#' @export
new_assertion <- function(check_fn) {
  if (!is.function(check_fn)) {
    cli::cli_abort("{.arg check_fn} must be a function")
  }
  structure(list(check = check_fn), class = "adRutils_assertion")
}


#' Assert a single logical value (`TRUE` or `FALSE`, not `NA`)
#'
#' Use inside [validate_args()] to check that an argument is a single
#' non-missing logical value.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(quiet = is_flag())
#' }
#' @export
is_flag <- function() {
  new_assertion(function(x, nm) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      sprintf(
        "{.arg {nm}} must be a single TRUE or FALSE (got %s)",
        .describe_value(x)
      )
    }
  })
}


#' Assert a single non-empty string
#'
#' Use inside [validate_args()] to check that an argument is a single
#' non-missing, non-empty character value.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(group_var = is_string())
#' }
#' @export
is_string <- function() {
  new_assertion(function(x, nm) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      sprintf(
        "{.arg {nm}} must be a single non-empty string (got %s)",
        .describe_value(x)
      )
    }
  })
}


#' Assert a non-empty character vector
#'
#' Use inside [validate_args()] to check that an argument is a character
#' vector of length at least 1.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(vars = is_nonempty_character())
#' }
#' @export
is_nonempty_character <- function() {
  new_assertion(function(x, nm) {
    if (!is.character(x) || length(x) == 0L) {
      sprintf(
        "{.arg {nm}} must be a non-empty character vector (got %s)",
        .describe_value(x)
      )
    }
  })
}


#' Assert a single numeric value, optionally bounded
#'
#' Use inside [validate_args()] to check that an argument is a single
#' non-missing numeric value, optionally requiring it to be positive
#' and/or fall within a range.
#'
#' @param min,max Inclusive bounds. Use `-Inf` / `Inf` for unbounded.
#' @param positive If `TRUE`, also require `x > 0`.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(
#'   alpha = is_number(min = 0, max = 1),
#'   se    = is_number(positive = TRUE)
#' )
#' }
#' @export
is_number <- function(min = -Inf, max = Inf, positive = FALSE) {
  new_assertion(function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
      return(sprintf(
        "{.arg {nm}} must be a single numeric value (got %s)",
        .describe_value(x)
      ))
    }
    if (positive && x <= 0) {
      return(sprintf(
        "{.arg {nm}} must be positive (got %g)", x
      ))
    }
    if (x < min || x > max) {
      sprintf(
        "{.arg {nm}} must be between %g and %g (got %g)",
        min, max, x
      )
    }
  })
}


#' Assert one of a fixed set of values
#'
#' Use inside [validate_args()] to check that an argument matches one of
#' a set of allowed values.
#'
#' @param choices A vector of allowed values.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(method = is_one_of(c("bonferroni", "holm", "BH")))
#' }
#' @export
is_one_of <- function(choices) {
  new_assertion(function(x, nm) {
    if (length(x) != 1L || !x %in% choices) {
      sprintf(
        "{.arg {nm}} must be one of %s (got %s)",
        paste0('"', choices, '"', collapse = ", "),
        .describe_value(x)
      )
    }
  })
}


#' Assert a non-empty list
#'
#' Use inside [validate_args()] to check that an argument is a list of
#' length at least 1.
#'
#' @return An assertion object consumed by [validate_args()].
#' @examples
#' \dontrun{
#' validate_args(groups = is_nonempty_list())
#' }
#' @export
is_nonempty_list <- function() {
  new_assertion(function(x, nm) {
    if (!is.list(x) || length(x) == 0L) {
      sprintf(
        "{.arg {nm}} must be a non-empty list (got %s)",
        .describe_value(x)
      )
    }
  })
}


# --- internal helpers -------------------------------------------------------

#' Evaluate a custom check, catching errors and producing a useful message.
#' @keywords internal
#' @noRd
.eval_custom_check <- function(check, idx, envir) {
  # Two accepted shapes:
  #   1) custom_check() result: list(condition = quosure, message = ...)
  #   2) legacy bare list: list(condition = <evaluated logical>, message = ...)
  # We support shape 2 for backwards compatibility but recommend shape 1.
  if (inherits(check, "adRutils_custom_check")) {
    result <- tryCatch(
      rlang::eval_tidy(check$condition, env = envir),
      error = function(e) {
        return(structure(list(err = conditionMessage(e)), class = "eval_error"))
      }
    )
    if (inherits(result, "eval_error")) {
      return(c(
        check$message,
        sprintf("(check could not be evaluated: %s)", result$err)
      ))
    }
    if (!isTRUE(result)) return(check$message)
    return(NULL)
  }

  # Legacy shape: condition was already evaluated when the list was built.
  if (is.list(check) && !is.null(check$condition) && !is.null(check$message)) {
    if (!isTRUE(check$condition)) return(check$message)
    return(NULL)
  }

  cli::cli_abort(c(
    "Custom check #{idx} has an unrecognized shape.",
    "i" = "Use {.fn custom_check} to construct custom checks."
  ))
}

#' Infer the calling function's name for use in error context.
#' @keywords internal
#' @noRd
.infer_context <- function() {
  caller <- sys.call(-2L)
  if (is.null(caller)) return("function")
  fn_name <- tryCatch(as.character(caller[[1]]), error = function(e) "function")
  fn_name[1]
}

#' Look up a value in the calling function's environment.
#' @keywords internal
#' @noRd
.lookup_caller_value <- function(name, envir, context) {
  if (!exists(name, envir = envir, inherits = FALSE)) {
    cli::cli_abort(c(
      "In {.fn {context}}: validation references {.arg {name}}, but no such argument exists in the calling function.",
      "i" = "Check for typos, or pass {.arg .envir} explicitly if calling {.fn validate_args} from a wrapper."
    ))
  }
  get(name, envir = envir, inherits = FALSE)
}

#' Compact human-readable description of a value for error messages.
#' Truncates long structures so error messages stay readable.
#' @keywords internal
#' @noRd
.describe_value <- function(x) {
  if (is.null(x))            return("NULL")
  if (length(x) == 0L)       return(sprintf("empty %s", typeof(x)))
  if (length(x) == 1L && is.atomic(x)) {
    if (is.character(x))     return(if (is.na(x)) "NA" else sprintf('"%s"', x))
    if (is.logical(x))       return(as.character(x))
    return(format(x))
  }
  sprintf("a %s of length %d", class(x)[1], length(x))
}
