
#' Summarize NA values by column
#'
#' Creates a tidy summary of missing values in each column of a data frame.
#' The function calculates the count and percentage of NA values for each column
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
#' @export
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
summarize_na <- function(data,
                         desc_order = TRUE,
                         threshold = NULL,
                         include_count = TRUE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }

  if (!is.null(threshold) && (!is.numeric(threshold) || threshold < 0 || threshold > 100)) {
    stop("Threshold must be a numeric value between 0 and 100.")
  }

  # Calculate total number of rows
  total_rows <- nrow(data)

  # Calculate the count and percentage of missing values for each column
  na_counts <- colSums(is.na(data))
  na_percent <- (na_counts / total_rows) * 100

  # Create the result tibble
  if (include_count) {
    result <- tibble::tibble(
      column = names(na_percent),
      count_na = na_counts,
      percent_na = round(na_percent, 3),
      total_rows = total_rows
    )
  } else {
    result <- tibble::tibble(
      column = names(na_percent),
      percent_na = round(na_percent, 3)
    )
  }

  # Sort by percentage of NAs
  if (desc_order) {
    result <- dplyr::arrange(result, dplyr::desc(.data[["percent_na"]]))
  } else {
    result <- dplyr::arrange(result, .data[["percent_na"]])
  }

  # Filter by threshold if provided
  if (!is.null(threshold)) {
    result <- result[result$percent_na >= threshold, ]
  }

  return(result)
}

#' Remove columns with high percentages of NA values
#'
#' This function identifies and removes columns from a data frame that have
#' a percentage of NA values exceeding the specified threshold.
#'
#' @param data A data frame or tibble to process
#' @param threshold Numeric; columns with NA percentage greater than this value will be removed (default: 99)
#' @param quiet Logical; if TRUE, suppresses messages about which columns are being removed
#' @param return_info Logical; if TRUE, returns a list containing the processed data frame and information
#'                  about removed columns
#'
#' @return If return_info=FALSE (default), returns the data frame with high-NA columns removed.
#'         If return_info=TRUE, returns a list with elements:
#'         \item{data}{The processed data frame}
#'         \item{removed_cols}{Character vector of removed column names}
#'         \item{na_summary}{Tibble with NA percentage information for all columns}
#'
#' @export
#'
#' @examples
#' # Create a sample data frame with some columns having high NA percentages
#' df <- data.frame(
#'   id = 1:100,
#'   almost_empty = c(rep(NA, 99), 1),
#'   completely_empty = rep(NA, 100),
#'   mostly_filled = c(rep(NA, 20), 1:80)
#' )
#'
#' # Remove columns with more than 90% NA values
#' drop_sparse_na_cols(df, threshold = 90)
#'
#' # Get information about what was removed
#' result <- drop_sparse_na_cols(df, threshold = 90, return_info = TRUE)
#' result$removed_cols
#' result$na_summary
#'
#' @seealso \code{\link{summarize_na}} for analyzing NA patterns without removing columns
drop_sparse_na_cols <- function(data, threshold = 99, quiet = FALSE, return_info = FALSE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }

  if (!is.numeric(threshold) || threshold < 0 || threshold > 100) {
    stop("Threshold must be a numeric value between 0 and 100.")
  }

  # Get NA information for all columns
  # Assuming summarize_na is available, otherwise use the function directly
  if (exists("summarize_na", mode = "function")) {
    na_info <- summarize_na(data, desc_order = TRUE)
  } else {
    # Fallback to direct calculation if summarize_na isn't available
    na_percent <- colMeans(is.na(data)) * 100
    na_info <- tibble::tibble(
      column = names(na_percent),
      percent_na = round(na_percent, 3)
    )
    na_info <- dplyr::arrange(na_info, dplyr::desc(.data[["percent_na"]]))
  }

  # Identify columns to remove
  cols_to_remove <- na_info$column[na_info$percent_na > threshold]

  # Process the data frame
  if (length(cols_to_remove) > 0) {
    if (!quiet) {
      message("Removing ", length(cols_to_remove), " column(s) with >", threshold,
              "% missing values: ", paste(cols_to_remove, collapse = ", "))
    }
    # Remove columns
    data_clean <- data[, !(names(data) %in% cols_to_remove), drop = FALSE]
  } else {
    if (!quiet) {
      message("No columns with >", threshold, "% missing values found.")
    }
    data_clean <- data
  }

  # Return results
  if (return_info) {
    return(list(
      data = data_clean,
      removed_cols = cols_to_remove,
      na_summary = na_info
    ))
  } else {
    return(data_clean)
  }
}

