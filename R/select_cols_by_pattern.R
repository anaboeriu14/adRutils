#' Select columns by name pattern
#'
#' This function selects columns from a data frame based on regex patterns matching column names.
#' It is a convenience wrapper around dplyr's select() and matches() functions with additional
#' validation and flexibility.
#'
#' @param dataf A data frame or tibble to select columns from
#' @param patterns Character vector of regex patterns to match column names
#' @param exclude Character vector of regex patterns to exclude (optional)
#' @param ignore_case Logical; if TRUE (default), performs case-insensitive matching
#' @param warn_no_match Logical; if TRUE (default), warns instead of errors when no columns match
#' @param invert Logical; if TRUE, selects columns that do NOT match the patterns
#'
#' @return A data frame or tibble containing only the selected columns
#'
#' @export
#'
#' @examples
#' # Create a sample data frame
#' test_df <- data.frame(
#'   id = 1:5,
#'   name_first = c("John", "Jane", "Bob", "Alice", "Tom"),
#'   name_last = c("Smith", "Doe", "Johnson", "Brown", "Wilson"),
#'   age = c(25, 30, 35, 40, 45),
#'   score_math = c(85, 90, 78, 92, 88),
#'   score_eng = c(76, 94, 82, 88, 79)
#' )
#'
#' # Select columns that contain "name"
#' select_cols_by_pattern(test_df, "name")
#'
#' # Select columns that contain "score" but exclude "math"
#' select_cols_by_pattern(test_df, "score", exclude = "math")
#'
#' # Select all columns EXCEPT those containing "score"
#' select_cols_by_pattern(test_df, "score", invert = TRUE)
#'
#' # Multiple patterns
#' select_cols_by_pattern(test_df, c("id", "age"))
#
#' @importFrom dplyr select matches
#' @importFrom rlang .data
select_cols_by_pattern <- function(dataf, patterns, exclude = NULL, ignore_case = TRUE,
                                   warn_no_match = TRUE, invert = FALSE) {
  # Input validation
  if (!inherits(dataf, c("data.frame", "tbl"))) {
    stop("The 'dataf' argument must be a data frame or tibble.")
  }

  if (!is.character(patterns) || length(patterns) == 0) {
    stop("The 'patterns' argument must be a non-empty character vector of regular expression patterns.")
  }

  # Create a single regex pattern from the patterns vector
  combined_pattern <- paste(patterns, collapse = "|")

  # Handle case sensitivity
  if (ignore_case) {
    # Use regex flag for case insensitivity
    combined_pattern <- paste0("(?i)", combined_pattern)
  }

  # Find potential matches before applying exclusions
  potential_matches <- grep(combined_pattern, names(dataf), value = TRUE)

  # Apply exclusion patterns if provided
  if (!is.null(exclude) && length(exclude) > 0) {
    exclude_pattern <- paste(exclude, collapse = "|")
    if (ignore_case) {
      exclude_pattern <- paste0("(?i)", exclude_pattern)
    }
    final_matches <- potential_matches[!grepl(exclude_pattern, potential_matches)]
  } else {
    final_matches <- potential_matches
  }

  # Check if any columns match after exclusions
  if (length(final_matches) == 0) {
    msg <- paste0("No columns matched the pattern(s): ",
                  paste(patterns, collapse = ", "))
    if (!is.null(exclude)) {
      msg <- paste0(msg, " after excluding: ", paste(exclude, collapse = ", "))
    }

    if (warn_no_match) {
      warning(msg)
      return(dataf[, integer(0), drop = FALSE])  # Return empty data frame with same row count
    } else {
      stop(msg)
    }
  }

  # Select columns based on pattern matching
  if (invert) {
    # Select columns that DON'T match the patterns
    result <- dataf[, !(names(dataf) %in% final_matches), drop = FALSE]
  } else {
    # Select columns that match the patterns
    result <- dataf[, final_matches, drop = FALSE]
  }

  # Return the subset data frame
  return(result)
}
