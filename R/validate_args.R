#' Validate function inputs with consistent error messages
#'
#' Runs a set of named assertions against function arguments and reports
#' all failures together. Designed to replace ad-hoc validation scattered
#' through individual functions with a single, uniform interface.
#'
#' @details
#' All conditions in `custom_checks` are evaluated eagerly when the list is
#' constructed (not lazily inside `validate_args`). Write each `condition`
#' so that it is safe to evaluate even when its referent might be `NULL`,
#' typically by guarding with `is.null()` and `&&`:
#'
#' ```r
#' list(
#'   condition = !is.null(x) && is.character(x) && length(x) > 0,
#'   message   = "{.arg x} must be a non-empty character vector"
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
#' @param custom_checks A list of `list(condition = <logical>, message = <chr>)`
#'   pairs for domain-specific checks that don't fit the standard shapes.
#' @param context Calling function name. Defaults to the calling function
#'   detected via `sys.call()`.
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
#'     quiet           = is_flag()
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
                          context         = NULL) {

  context <- context %||% .infer_context()
  errors  <- character()

  # --- arg-shape assertions via ... ---
  dots      <- rlang::enquos(..., .ignore_empty = "all")
  dot_names <- names(dots)

  for (i in seq_along(dots)) {
    nm        <- dot_names[i]
    assertion <- rlang::eval_tidy(dots[[i]])

    if (is.null(nm) || !nzchar(nm)) {
      cli::cli_abort(
        "In {.fn {context}()}: assertions passed via {.code ...} must be named."
      )
    }
    if (!inherits(assertion, "adRutils_assertion")) {
      cli::cli_abort(
        "In {.fn {context}()}: {.arg {nm}} must be an assertion helper, not a bare value."
      )
    }

    value <- .lookup_caller_value(nm)
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

  # --- custom checks ---
  for (check in custom_checks) {
    if (!isTRUE(check$condition)) {
      errors <- c(errors, check$message)
    }
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "In {.fn {context}()}: {length(errors)} validation issue{?s} found.",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  invisible(TRUE)
}


# --- assertion constructors -------------------------------------------------
# Each public assertion helper returns an object of class
# "adRutils_assertion" with a $check method that takes (value, arg_name)
# and returns NULL on success or an error message string.
# `new_assertion()` is the internal constructor.

#' @keywords internal
#' @noRd
new_assertion <- function(check_fn) {
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
      "{.arg {nm}} must be a single TRUE or FALSE"
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
      "{.arg {nm}} must be a single non-empty string"
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
      "{.arg {nm}} must be a non-empty character vector"
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
      return("{.arg {nm}} must be a single numeric value")
    }
    if (positive && x <= 0) {
      return("{.arg {nm}} must be positive")
    }
    if (x < min || x > max) {
      sprintf("{.arg {nm}} must be between %g and %g", min, max)
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
        "{.arg {nm}} must be one of: %s",
        paste0('"', choices, '"', collapse = ", ")
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
      "{.arg {nm}} must be a non-empty list"
    }
  })
}


# --- internal helpers -------------------------------------------------------

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
.lookup_caller_value <- function(name) {
  caller_env <- parent.frame(2L)
  if (!exists(name, envir = caller_env, inherits = FALSE)) {
    cli::cli_abort(
      "Validation references {.arg {name}}, but no such argument exists in the calling function."
    )
  }
  get(name, envir = caller_env, inherits = FALSE)
}
