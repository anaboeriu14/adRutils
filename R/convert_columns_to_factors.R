#' Convert columns to factors based on name patterns
#'
#' Converts columns in a data frame to factors based on matching patterns
#' in their names. Can create either regular or ordered factors.
#'
#' @param dataf A data frame containing the columns to convert
#' @param patterns Character vector of patterns to match column names against
#' @param exclude Optional character vector of patterns to exclude from matching
#' @param ordered Logical indicating whether to create ordered factors (default: FALSE)
#' @param quiet Logical indicating whether to suppress messages (default: FALSE)
#'
#' @return A data frame with specified columns converted to factors
#'
#' @examples
#' testDf <- data.frame(
#'   cdx_var1 = c("A", "B", "A", "C"),
#'   cdx_var2 = c("X", "Y", "X", "Z"),
#'   age = c(25, 30, 22, 40),
#'   gender = c("M", "M", "F", "M")
#' )
#' newD <- convert_columns_to_factors(testDf, c("cdx", "gender"))
#' str(newD)
#'
#' @export
convert_columns_to_factors <- function(dataf, patterns, exclude = NULL,
                                       ordered = FALSE, quiet = FALSE) {

  # Validate inputs
  validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = is.character(patterns) && length(patterns) > 0,
        message = "{.arg patterns} must be a non-empty character vector"
      ),
      list(
        condition = is.logical(ordered) && length(ordered) == 1,
        message = "{.arg ordered} must be a single logical value"
      ),
      list(
        condition = is.logical(quiet) && length(quiet) == 1,
        message = "{.arg quiet} must be a single logical value"
      )
    ),
    context = "convert_columns_to_factors"
  )

  # Find matching columns
  matching_cols <- .find_cols_for_conversion(dataf, patterns, exclude)

  if (length(matching_cols) == 0) {
    if (!quiet) {
      .warn_no_matches(patterns, exclude)
    }
    return(dataf)
  }

  # Apply factor conversion to each matching column
  result <- dataf
  for (col in matching_cols) {
    result[[col]] <- .to_factor(dataf[[col]], ordered)
  }

  # Report success
  if (!quiet) {
    .report_conversion(matching_cols)
  }

  return(result)
}

#' Find columns for factor conversion (case-insensitive)
#' @keywords internal
.find_cols_for_conversion <- function(dataf, patterns, exclude) {
  # Build combined regex pattern
  combined_pattern <- paste(patterns, collapse = "|")

  # Case-insensitive matching by default
  matching_cols <- grep(combined_pattern, names(dataf),
                        value = TRUE, ignore.case = TRUE)

  # Apply exclusions if provided
  if (!is.null(exclude) && length(exclude) > 0) {
    exclude_pattern <- paste(exclude, collapse = "|")
    matching_cols <- matching_cols[!grepl(exclude_pattern, matching_cols,
                                          ignore.case = TRUE)]
  }

  return(matching_cols)
}

#' Convert a single column to factor
#' @keywords internal
.to_factor <- function(x, ordered) {
  if (is.factor(x)) {
    # Convert existing factor to ordered if needed
    if (ordered && !is.ordered(x)) {
      return(factor(x, levels = levels(x), ordered = TRUE))
    }
    return(x)  # Already correct type
  }

  # Convert non-factor to factor
  return(factor(x, ordered = ordered))
}

#' Warn when no columns match patterns
#' @keywords internal
.warn_no_matches <- function(patterns, exclude) {
  if (!is.null(exclude)) {
    cli::cli_alert_warning(
      "No columns matching {.val {patterns}} after excluding {.val {exclude}}"
    )
  } else {
    cli::cli_alert_warning("No columns matching {.val {patterns}}")
  }

  invisible(NULL)
}

#' Report successful conversion
#' @keywords internal
.report_conversion <- function(cols) {
  n_cols <- length(cols)

  if (n_cols <= 8) {
    cli::cli_alert_success(
      "Converted {n_cols} column{?s} to factor{?s}: {.field {cols}}"
    )
  } else {
    first_cols <- head(cols, 5)
    remaining <- n_cols - 5
    cli::cli_alert_success(
      "Converted {n_cols} columns to factors: {.field {first_cols}} and {remaining} more"
    )
  }

  invisible(NULL)
}
