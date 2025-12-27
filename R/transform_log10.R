#' Transform numeric variables to log10 scale
#'
#' Transforms specified numeric variables to log10 scale and adds them
#' as new columns with "log10_" prefix. Includes validation checks to ensure
#' proper transformation and prevents double transformation.
#'
#' @param dataf A data frame containing the variables to transform
#' @param vars Character vector of column names to transform
#' @param force Logical. If TRUE, bypasses the check for previous processing
#' @param quiet Logical. If TRUE, suppresses warning messages
#'
#' @return A data frame with the original columns plus new log10-transformed columns
#'
#' @examples
#' \dontrun{
#' test_df <- data.frame(
#'   id = 1:3,
#'   biomarker1 = c(10, 20, 30),
#'   biomarker2 = c(5, 15, 25),
#'   age = c(67, 97, 34),
#'   biomarker3 = c(1.2, -0.23, 3)
#' )
#'
#' # Valid transformation
#' test_df2 <- transform_log10(test_df, c("biomarker1", "biomarker2"))
#'
#' # This will error (negative values)
#' err_result <- transform_log10(test_df, c("age", "biomarker3"))
#' }
#' @export
transform_log10 <- function(dataf, vars, force = FALSE, quiet = FALSE) {

  # Validate inputs
  .validate_log10_params(dataf, vars)

  # Check for already log-transformed column names
  if (!quiet) {
    .warn_if_log_named(vars)
  }

  # Check if already processed
  if (!force) {
    is_processed("transform_log10", vars, error_if_exists = TRUE)
  }

  # Perform transformation
  result <- .apply_log10_transform(dataf, vars, quiet)

  # Register as processed
  register_processed("transform_log10", vars)

  return(result)
}

#' Validate transform_log10 parameters
#' @keywords internal
.validate_log10_params <- function(dataf, vars) {
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

  # Check for negative or zero values
  invalid_vars <- .find_invalid_values(dataf, vars)
  if (length(invalid_vars) > 0) {
    cli::cli_abort(
      c(
        "Cannot apply log10 transformation:",
        "x" = "The following column{?s} contain{?/s} negative or zero values: {.field {invalid_vars}}"
      )
    )
  }

  invisible(TRUE)
}

#' Find variables with invalid values for log transformation
#' @keywords internal
.find_invalid_values <- function(dataf, vars) {
  vars[vapply(dataf[vars], function(x) any(x <= 0, na.rm = TRUE), logical(1))]
}

#' Warn if variables already have log10 naming convention
#' @keywords internal
.warn_if_log_named <- function(vars) {
  potential_log_cols <- vars[grepl("^log10_", vars)]

  if (length(potential_log_cols) > 0) {
    cli::cli_alert_warning(
      "Column{?s} already ha{?s/ve} log10 naming convention: {.field {potential_log_cols}}"
    )
  }

  invisible(NULL)
}

#' Apply log10 transformation to variables
#' @keywords internal
.apply_log10_transform <- function(dataf, vars, quiet) {
  result <- dataf
  overwritten <- character()

  for (var in vars) {
    log_col_name <- paste0("log10_", var)

    # Track if overwriting
    if (log_col_name %in% names(dataf)) {
      overwritten <- c(overwritten, log_col_name)
    }

    result[[log_col_name]] <- log10(dataf[[var]])
  }

  # Warn about overwrites
  if (length(overwritten) > 0 && !quiet) {
    cli::cli_alert_warning("Overwriting existing column{?s}: {.field {overwritten}}")
  }

  return(result)
}
