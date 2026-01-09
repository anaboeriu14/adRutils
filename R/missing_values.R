#' Summarize NA values by column
#'
#' Creates a tidy summary of missing values in each column of a data frame.
#' Calculates the count and percentage of NA values for each column
#' and returns a sorted tibble.
#'
#' @param data A data frame or tibble to analyze for missing values
#' @param desc_order Logical; if TRUE (default), sorts from highest to lowest percent of NAs
#' @param threshold Numeric; if provided, returns only columns with NA percentage above this threshold
#' @param include_count Logical; if TRUE (default), includes count of NAs in addition to percentages
#'
#' @return A tibble with columns:
#'   \item{column}{Name of the column}
#'   \item{count_na}{Number of NA values (if include_count=TRUE)}
#'   \item{percent_na}{Percentage of values that are NA (rounded to 3 decimal places)}
#'   \item{total_rows}{Total number of rows in the data (if include_count=TRUE)}
#'
#' @examples
#' # Create a sample data frame with missing values
#' testDF <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c(NA, NA, 3, 4, 5),
#'   c = 1:5
#' )
#'
#' # Summarize missing values
#' summarize_na(testDF)
#'
#' # Get only columns with >= 50% missing values
#' summarize_na(testDF, threshold = 50)
#'
#' # Sort by lowest percentage first
#' summarize_na(testDF, desc_order = FALSE)
#'
#' @export
summarize_na <- function(data,
                         desc_order = TRUE,
                         threshold = NULL,
                         include_count = TRUE) {
  # Validate inputs
  validate_params(
    data = data,
    custom_checks = list(
      list(
        condition = is.null(threshold) || (is.numeric(threshold) && threshold >= 0 && threshold <= 100),
        message = "{.arg threshold} must be a numeric value between 0 and 100"
      )
    ),
    context = "summarize_na"
  )

  # Calculate NA statistics
  total_rows <- nrow(data)
  na_counts <- colSums(is.na(data))
  na_percent <- round((na_counts / total_rows) * 100, 3)

  # Build result tibble
  result <- if (include_count) {
    tibble::tibble(
      column = names(na_percent),
      count_na = na_counts,
      percent_na = na_percent,
      total_rows = total_rows
    )
  } else {
    tibble::tibble(
      column = names(na_percent),
      percent_na = na_percent
    )
  }

  # Sort
  result <- dplyr::arrange(
    result,
    if (desc_order) dplyr::desc(.data[["percent_na"]]) else .data[["percent_na"]]
  )

  # Filter by threshold if provided
  if (!is.null(threshold)) {
    result <- dplyr::filter(result, .data[["percent_na"]] >= threshold)
  }

  return(result)
}

#' Remove columns with high percentages of NA values
#'
#' Identifies and removes columns from a data frame that have
#' a percentage of NA values greater than or equal to the specified threshold.
#'
#' @param data A data frame or tibble to process
#' @param threshold Numeric; columns with NA percentage >= this value will be removed (default: 99)
#' @param quiet Logical; if TRUE, suppresses messages
#' @param return_info Logical; if TRUE, returns a list with additional information
#'
#' @return If return_info=FALSE (default), returns the data frame with high-NA columns removed.
#'   If return_info=TRUE, returns a list with elements:
#'   \item{data}{The processed data frame}
#'   \item{removed_cols}{Character vector of removed column names}
#'   \item{na_summary}{Tibble with NA percentage information for all columns}
#'
#' @examples
#' testD <- data.frame(
#'   id = 1:100,
#'   almost_empty = c(rep(NA, 99), 1),
#'   completely_empty = rep(NA, 100),
#'   mostly_filled = c(rep(NA, 20), 1:80)
#' )
#'
#' # Remove columns with more than 90% NA values
#' drop_sparse_na_cols(testD, threshold = 90)
#'
#' # Get information about what was removed
#' result <- drop_sparse_na_cols(testD, threshold = 90, return_info = TRUE)
#'
#' @seealso \code{\link{summarize_na}} for analyzing NA patterns without removing columns
#' @export
drop_sparse_na_cols <- function(data, threshold = 99, quiet = FALSE, return_info = FALSE) {

  validate_params(
    data = data,
    custom_checks = list(
      list(
        condition = is.numeric(threshold) && threshold >= 0 && threshold <= 100,
        message = "{.arg threshold} must be a numeric value between 0 and 100"
      )
    ),
    context = "drop_sparse_na_cols"
  )

  # Get NA summary
  na_info <- summarize_na(data, desc_order = TRUE)
  cols_to_remove <- na_info$column[na_info$percent_na >= threshold]

  # Remove columns and report
  if (length(cols_to_remove) > 0) {
    if (!quiet) {
      cli::cli_alert_info(
        "Removing {length(cols_to_remove)} column{?s} with >= {threshold}% missing values"
      )
    }
    data_clean <- data[, !(names(data) %in% cols_to_remove), drop = FALSE]
  } else {
    if (!quiet) {
      cli::cli_alert_success("No columns with >= {threshold}% missing values found")
    }
    data_clean <- data
  }

  # Return in requested format
  if (return_info) {
    return(list(
      data = data_clean,
      removed_cols = cols_to_remove,
      na_summary = na_info
    ))
  }

  return(data_clean)
}
