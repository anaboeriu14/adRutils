#' Base Input Validation
#'
#' A streamlined validation function based on common validation patterns.
#' Handles the most common validation scenarios found across functions.
#'
#' @param data A data frame to validate
#' @param columns Character vector of column names that must exist in data
#' @param numeric_columns Character vector of columns that must be numeric
#' @param grouping_vars Character vector of grouping variables (optional)
#' @param method Character string for method validation (optional)
#' @param valid_methods Character vector of allowed methods
#' @param custom_checks List of custom validation checks. Must be in the form condition, message
#' @param context Character string describing the calling function
#'
#' @return Invisibly returns TRUE if all validations pass, stops with error otherwise
#' @export
validate_params <- function(data = NULL,
                            columns = NULL,
                            numeric_columns = NULL,
                            grouping_vars = NULL,
                            method = NULL,
                            valid_methods = NULL,
                            custom_checks = list(),
                            context = "function") {
  # Data frame validation with context
  if (!is.null(data) && !is.data.frame(data)) {
    cli::cli_abort("In {.fn {context}()}: Input {.arg data} must be a data frame")
  }

  # Column existence with context
  if (!is.null(columns) && !is.null(data)) {
    missing_cols <- columns[!columns %in% names(data)]
    if (length(missing_cols) > 0) {
      cli::cli_abort("In {.fn {context}()}: The following column{?s} do not exist in the data: {.val {missing_cols}}")
    }
  }

  # Numeric column validation
  if (!is.null(numeric_columns) && !is.null(data)) {
    non_numeric <- character()
    for (col in numeric_columns) {
      if (col %in% names(data) && !is.numeric(data[[col]])) {
        non_numeric <- c(non_numeric, col)
      }
    }
    if (length(non_numeric) > 0) {
      cli::cli_abort("In {.fn {context}()}: The following column{?s} {?is/are} not numeric: {.val {non_numeric}}")
    }
  }

  # Grouping variables validation
  if (!is.null(grouping_vars) && !is.null(data)) {
    missing_groups <- grouping_vars[!grouping_vars %in% names(data)]
    if (length(missing_groups) > 0) {
      cli::cli_abort("In {.fn {context}()}: The following grouping variable{?s} do not exist in the data: {.val {missing_groups}}")
    }
  }

  # Method validation
  if (!is.null(method) && !is.null(valid_methods)) {
    if (!method %in% valid_methods) {
      cli::cli_abort("In {.fn {context}()}: {.arg method} must be one of: {.val {valid_methods}}")
    }
  }

  # Custom checks
  if (length(custom_checks) > 0) {
    for (i in seq_along(custom_checks)) {
      check <- custom_checks[[i]]
      if (!check$condition) {
        cli::cli_abort("In {.fn {context}()}: {check$message}")
      }
    }
  }

  invisible(TRUE)
}
