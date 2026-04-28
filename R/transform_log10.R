#' Transform numeric variables to a log10 scale
#'
#' Adds log10-transformed copies of `vars` to `dataf`, prefixed with
#' `log10_`. Original columns are preserved unchanged.
#'
#' @details
#' Errors when any value in `vars` is `<= 0`, since `log10()` is undefined
#' there. Errors when a target column (`log10_<var>`) already exists,
#' unless `overwrite = TRUE`.
#'
#' Warns (does not error) if an input variable is itself already
#' `log10_`-prefixed, since this typically indicates double-transformation.
#'
#' @param dataf A data frame.
#' @param vars Character vector of column names to transform. Must be
#'   numeric and strictly positive.
#' @param overwrite If `TRUE`, replace existing `log10_<var>` columns.
#'   Default `FALSE`.
#' @param quiet If `TRUE`, suppress warnings. Default `FALSE`.
#'
#' @return `dataf` with one new column per input variable, named
#'   `log10_<var>`.
#'
#' @examples
#' df <- data.frame(x = c(10, 100, 1000), y = c(1, 2, 3))
#' transform_log10(df, vars = c("x", "y"))
#'
#' @export
transform_log10 <- function(dataf, vars, overwrite = FALSE, quiet = FALSE) {

  validate_args(
    data            = dataf,
    columns         = vars,
    numeric_columns = vars,
    vars            = is_nonempty_character(),
    overwrite       = is_flag(),
    quiet           = is_flag()
  )

  # Single-pass classification: decide everything before mutating.
  new_cols      <- paste0("log10_", vars)
  has_nonpos    <- vapply(vars, function(v) any(dataf[[v]] <= 0, na.rm = TRUE),
                          logical(1))
  already_log   <- grepl("^log10_", vars)
  exists_target <- new_cols %in% names(dataf)

  if (any(has_nonpos)) {
    cli::cli_abort(
      "Column{?s} with non-positive values: {.field {vars[has_nonpos]}}"
    )
  }
  if (any(exists_target) && !overwrite) {
    cli::cli_abort(c(
      "Target column{?s} already exist: {.field {new_cols[exists_target]}}",
      "i" = "Use {.arg overwrite = TRUE} to replace."
    ))
  }
  if (!quiet && any(already_log)) {
    cli::cli_warn("Input{?s} already log10-prefixed: {.field {vars[already_log]}}")
  }

  dataf[new_cols] <- lapply(vars, function(v) log10(dataf[[v]]))

  if (!quiet && any(exists_target)) {
    cli::cli_warn("Overwrote column{?s}: {.field {new_cols[exists_target]}}")
  }

  dataf
}
