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
#' @param custom_checks List of custom validation checks
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
    stop("In ", context, "(): Input 'data' must be a data frame.", call. = FALSE)
  }

  # Column existence with context
  if (!is.null(columns) && !is.null(data)) {
    missing_cols <- columns[!columns %in% names(data)]
    if (length(missing_cols) > 0) {
      stop("In ", context, "(): The following columns do not exist in the data: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  # 3. Numeric column validation
  if (!is.null(numeric_columns) && !is.null(data)) {
    non_numeric <- character()
    for (col in numeric_columns) {
      if (col %in% names(data) && !is.numeric(data[[col]])) {
        non_numeric <- c(non_numeric, col)
      }
    }
    if (length(non_numeric) > 0) {
      stop("In ", context, "(): The following columns are not numeric: ",
           paste(non_numeric, collapse = ", "), call. = FALSE)
    }
  }

  # 4. Grouping variables validation
  if (!is.null(grouping_vars) && !is.null(data)) {
    missing_groups <- grouping_vars[!grouping_vars %in% names(data)]
    if (length(missing_groups) > 0) {
      stop("In ", context, "(): The following grouping variables do not exist in the data: ",
           paste(missing_groups, collapse = ", "), call. = FALSE)
    }
  }

  # 5. Method validation
  if (!is.null(method) && !is.null(valid_methods)) {
    if (!method %in% valid_methods) {
      stop("In ", context, "(): Method must be one of: ", paste(valid_methods, collapse = ", "), call. = FALSE)
    }
  }

  # 6. Custom checks
  if (length(custom_checks) > 0) {
    for (i in seq_along(custom_checks)) {
      check <- custom_checks[[i]]
      if (!check$condition) {
        stop("In ", context, "(): ", check$message, call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

