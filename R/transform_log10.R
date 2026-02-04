#' Transform Numeric Variables to Log10 Scale
#'
#' Adds log10-transformed columns with "log10_" prefix. Errors on non-positive values.
#'
#' @param dataf Data frame containing variables to transform
#' @param vars Character vector of column names to transform
#' @param overwrite Allow overwriting existing log10_ columns (default: FALSE)
#' @param quiet Suppress messages (default: FALSE)
#'
#' @return Data frame with added log10-transformed columns
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = c(10, 100, 1000), y = c(1, 2, 3))
#' transform_log10(df, c("x", "y"))
#' }
#' @export
transform_log10 <- function(dataf, vars, overwrite = FALSE, quiet = FALSE) {

  # Validate
  validate_params(
    data = dataf,
    columns = vars,
    numeric_columns = vars,
    custom_checks = list(
      list(
        condition = is.character(vars) && length(vars) > 0,
        message = "{.arg vars} must be a non-empty character vector"
      )
    ),
    context = "transform_log10"
  )

  # Check for non-positive values
  invalid_vars <- vars[vapply(vars, function(v) any(dataf[[v]] <= 0, na.rm = TRUE), logical(1))]
  if (length(invalid_vars) > 0) {
    cli::cli_abort("Column{?s} with non-positive values: {.field {invalid_vars}}")
  }

  # Warn if input vars already have log10_ prefix
  log_named <- vars[grepl("^log10_", vars)]
  if (!quiet && length(log_named) > 0) {
    cli::cli_warn("Column{?s} already named with log10_ prefix: {.field {log_named}}")
  }

  # Transform
  result <- dataf
  overwritten <- character()

  for (var in vars) {
    new_col <- paste0("log10_", var)

    if (new_col %in% names(dataf)) {
      if (!overwrite) {
        cli::cli_abort("Column {.field {new_col}} already exists. Use {.arg overwrite = TRUE} to replace.")
      }
      overwritten <- c(overwritten, new_col)
    }

    result[[new_col]] <- log10(dataf[[var]])
  }

  if (!quiet && length(overwritten) > 0) {
    cli::cli_warn("Overwrote column{?s}: {.field {overwritten}}")
  }

  result
}
