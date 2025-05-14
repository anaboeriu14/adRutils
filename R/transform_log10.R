#' Transform numeric variables to log10 scale
#'
#' This function transforms specified numeric variables to log10 scale and adds them
#' as new columns with "log10_" prefix. It includes several validation checks to ensure
#' proper transformation. Prevents double transformation ie log10(log10(x))
#'
#' @param dataf A data frame containing the variables to transform
#' @param vars Character vector of column names to transform
#' @param force Logical. If TRUE, bypasses the check for previous processing
#' @return A data frame with the original columns plus new log10-transformed columns
#' @export
#'
#' @examples
#'  \dontrun{
#' test_df <- data.frame(id = 1:3, biomarker1 = c(10, 20, 30),
#' biomarker2 = c(5, 15, 25), age = c(67,97,34), biomarker3 = c(1.2, -0.23, 3)
#' )
#' test_df2 <- transform_log10(test_df, c("biomarker1", "biomarker2"))
#' err_result = transform_log10(test_df, c("age", "biomarker3"))
#' }
transform_log10 <- function(dataf, vars, force = FALSE) {
  # Input validation
  if (!is.data.frame(dataf)) {
    stop("The 'dataf' argument must be a data frame.")
  }

  # Check if all columns exist in the data
  missing_columns <- vars[!vars %in% names(dataf)]
  if (length(missing_columns) > 0) {
    stop("Some specified columns do not exist in the data: ",
         paste(missing_columns, collapse = ", "))
  }

  # Check if all columns are numeric
  non_numeric_cols <- vars[!sapply(dataf[vars], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    stop("The following columns are NOT numeric: ",
         paste(non_numeric_cols, collapse = ", "))
  }

  # Check for negative or zero values
  neg_values <- vars[sapply(dataf[vars], function(x) any(x <= 0, na.rm = TRUE))]
  if (length(neg_values) > 0) {
    stop("The following columns contain negative or zero values: ",
         paste(neg_values, collapse = ", "))
  }

  # Check if columns have names suggesting they might be already log-transformed
  potential_log_cols <- vars[grepl("^log10_", vars)]
  if (length(potential_log_cols) > 0) {
    warning("Some columns appear to already have log10 naming convention: ",
            paste(potential_log_cols, collapse = ", "))
  }

  # Check if these variables have already been processed with transform_log10
  if (!force) {
    is_processed("transform_log10", vars, error_if_exists = TRUE)
  }

  # Perform log10 transformation
  data_add_log10 <- dataf
  for (var in vars) {
    log_col_name <- paste0("log10_", var)

    # This handles the case where somehow the column already exists without error
    if (log_col_name %in% names(dataf)) {
      warning("Column ", log_col_name, " already exists. Overwriting.")
    }

    data_add_log10[[log_col_name]] <- log10(dataf[[var]])
  }

  # Register these variables as processed
  register_processed("transform_log10", vars)

  return(data_add_log10)
}
