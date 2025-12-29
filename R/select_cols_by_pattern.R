#' Select columns by name pattern
#'
#' This function selects columns from a data frame based on regex patterns matching column names.
#' It is a convenience wrapper around dplyr's select() and matches() functions with additional
#' validation and flexibility.
#'
#' @param dataf A data frame or tibble to select columns from
#' @param patterns Character vector of regex patterns to match column names
#' @param exclude Character vector of regex patterns to exclude (optional)
#' @param ignore_case Logical. If TRUE (default), performs case-insensitive matching
#' @param warn_no_match Logical. If TRUE (default), warns instead of errors when no columns match
#' @param invert Logical. If TRUE, selects columns that do NOT match the patterns (default: FALSE)
#'
#' @return A data frame or tibble containing only the selected columns
#'
#' @details
#' The function uses regular expressions to match column names. Multiple patterns are combined
#' with OR logic (any pattern can match). Exclusion patterns are applied after initial matching.
#'
#' When \code{invert = TRUE}, the function returns columns that do NOT match any of the patterns.
#' This is useful for removing specific columns while keeping everything else.
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
#' # Multiple patterns (OR logic)
#' select_cols_by_pattern(test_df, c("id", "age"))
#'
#' # Case-sensitive matching
#' select_cols_by_pattern(test_df, "NAME", ignore_case = FALSE)
#'
#'
#' @export
select_cols_by_pattern <- function(dataf, patterns, exclude = NULL,
                                   ignore_case = TRUE, warn_no_match = TRUE,
                                   invert = FALSE) {

  # Validate inputs
  .validate_pattern_params(dataf, patterns, exclude, ignore_case,
                           warn_no_match, invert)

  # Find matching columns
  matched_cols <- .find_matching_columns(dataf, patterns, exclude, ignore_case)

  # Handle no matches
  if (length(matched_cols) == 0) {
    return(.handle_no_matches(dataf, patterns, exclude, warn_no_match))
  }

  # Select columns based on invert flag
  if (invert) {
    # Return columns that DON'T match
    return(dataf[, !(names(dataf) %in% matched_cols), drop = FALSE])
  } else {
    # Return columns that DO match
    return(dataf[, matched_cols, drop = FALSE])
  }
}

#' Validate select_cols_by_pattern parameters
#' @keywords internal
.validate_pattern_params <- function(dataf, patterns, exclude, ignore_case,
                                     warn_no_match, invert) {
  validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = is.character(patterns) && length(patterns) > 0,
        message = "{.arg patterns} must be a non-empty character vector of regular expression patterns"
      ),
      list(
        condition = is.null(exclude) || is.character(exclude),
        message = "{.arg exclude} must be NULL or a character vector"
      ),
      list(
        condition = is.logical(ignore_case) && length(ignore_case) == 1,
        message = "{.arg ignore_case} must be a single logical value"
      ),
      list(
        condition = is.logical(warn_no_match) && length(warn_no_match) == 1,
        message = "{.arg warn_no_match} must be a single logical value"
      ),
      list(
        condition = is.logical(invert) && length(invert) == 1,
        message = "{.arg invert} must be a single logical value"
      )
    ),
    context = "select_cols_by_pattern"
  )

  invisible(TRUE)
}

#' Find columns matching patterns with optional exclusions
#' @keywords internal
.find_matching_columns <- function(dataf, patterns, exclude, ignore_case) {
  # Build combined regex pattern
  combined_pattern <- paste(patterns, collapse = "|")

  # Add case-insensitive flag if needed
  if (ignore_case) {
    combined_pattern <- paste0("(?i)", combined_pattern)
  }

  # Find potential matches
  potential_matches <- grep(combined_pattern, names(dataf),
                            value = TRUE, perl = TRUE)

  # Apply exclusion patterns if provided
  if (!is.null(exclude) && length(exclude) > 0) {
    final_matches <- .apply_exclusions(potential_matches, exclude, ignore_case)
  } else {
    final_matches <- potential_matches
  }

  return(final_matches)
}

#' Apply exclusion patterns to matched columns
#' @keywords internal
.apply_exclusions <- function(matched_cols, exclude_patterns, ignore_case) {
  exclude_pattern <- paste(exclude_patterns, collapse = "|")

  if (ignore_case) {
    exclude_pattern <- paste0("(?i)", exclude_pattern)
  }

  # Keep columns that DON'T match exclusion pattern
  matched_cols[!grepl(exclude_pattern, matched_cols, perl = TRUE)]
}

#' Handle case when no columns match
#' @keywords internal
.handle_no_matches <- function(dataf, patterns, exclude, warn_no_match) {
  # Build informative message
  msg <- paste0(
    "No columns matched the pattern(s): ",
    paste(patterns, collapse = ", ")
  )

  if (!is.null(exclude)) {
    msg <- paste0(msg, " after excluding: ", paste(exclude, collapse = ", "))
  }

  # Warn or error based on user preference
  if (warn_no_match) {
    cli::cli_alert_warning(msg)
    return(dataf[, integer(0), drop = FALSE])  # Empty data frame with same rows
  } else {
    cli::cli_abort(msg)
  }
}
