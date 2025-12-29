#' Transform numeric variables to log10 scale
#'
#' Transforms specified numeric variables to log10 scale and adds them
#' as new columns with "log10_" prefix. Includes validation checks to ensure
#' proper transformation and prevents double transformation.
#'
#' @param dataf A data frame containing the variables to transform
#' @param vars Character vector of column names to transform
#' @param overwrite Logical. If TRUE, bypasses the check for previous processing (default: FALSE)
#' @param quiet Logical. If TRUE, suppresses informational messages (default: FALSE)
#'
#' @return A data frame with the original columns plus new log10-transformed columns
#'
#' @details
#' The function performs several validation checks:
#' \itemize{
#'   \item All variables must be numeric
#'   \item No values can be negative or zero (log10 undefined)
#'   \item Warns if variables already have "log10_" prefix
#'   \item Prevents double transformation (unless overwrite = TRUE)
#' }
#'
#' New columns are named with "log10_" prefix. For example, transforming
#' "biomarker1" creates a new column "log10_biomarker1".
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
#' # Check results
#' summary(test_df2$log10_biomarker1)
#'
#' # This will error (negative values in biomarker3)
#' err_result <- transform_log10(test_df, "biomarker3")
#'
#' # overwrite re-transformation
#' test_df3 <- transform_log10(test_df2, "biomarker1", overwrite = TRUE)
#'
#' # Quiet mode (suppress messages)
#' test_df4 <- transform_log10(test_df, "biomarker1", quiet = TRUE)
#' }
#' @export
transform_log10 <- function(dataf, vars, overwrite = FALSE, quiet = FALSE) {

  # Validate inputs
  .validate_log10_params(dataf, vars, overwrite, quiet)

  # Check for already log-transformed column names
  if (!quiet) {
    .warn_if_log_named(vars)
  }

  # Perform transformation
  result <- .apply_log10_transform(dataf, vars, overwrite, quiet)

  # Report success
  if (!quiet) {
    cli::cli_alert_success(
      "Transformed {length(vars)} variable{?s} to log10 scale"
    )
  }

  return(result)
}

#' Validate transform_log10 parameters
#' @keywords internal
.validate_log10_params <- function(dataf, vars, overwrite, quiet) {
  validate_params(
    data = dataf,
    columns = vars,
    numeric_columns = vars,
    custom_checks = list(
      list(
        condition = is.character(vars) && length(vars) > 0,
        message = "{.arg vars} must be a non-empty character vector"
      ),
      list(
        condition = is.logical(overwrite) && length(overwrite) == 1,
        message = "{.arg overwrite} must be a single logical value"
      ),
      list(
        condition = is.logical(quiet) && length(quiet) == 1,
        message = "{.arg quiet} must be a single logical value"
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
        "x" = "The following column{?s} contain{?/s} negative or zero values: {.field {invalid_vars}}",
        "i" = "log10 is only defined for positive values"
      )
    )
  }

  invisible(TRUE)
}

#' Find variables with invalid values for log transformation
#' @keywords internal
.find_invalid_values <- function(dataf, vars) {
  vars[vapply(vars, function(var) {
    any(dataf[[var]] <= 0, na.rm = TRUE)
  }, logical(1))]
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
.apply_log10_transform <- function(dataf, vars, overwrite, quiet) {
  result <- dataf
  overwritten <- character()

  for (var in vars) {
    log_col_name <- paste0("log10_", var)

    # Check if column already exists
    if (log_col_name %in% names(dataf)) {
      if (!overwrite) {
        cli::cli_abort(
          c(
            "Column {.field {log_col_name}} already exists",
            "i" = "This suggests {.field {var}} has already been log-transformed",
            "i" = "Use {.code overwrite = TRUE} to replace, or remove the column first"
          )
        )
      }
      overwritten <- c(overwritten, log_col_name)
    }

    # Apply transformation
    result[[log_col_name]] <- log10(dataf[[var]])
  }

  # Warn about overwrites
  if (length(overwritten) > 0 && !quiet) {
    cli::cli_alert_warning(
      "Overwriting existing column{?s}: {.field {overwritten}}"
    )
  }

  return(result)
}
