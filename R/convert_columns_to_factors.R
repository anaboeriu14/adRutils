#' Convert columns to factors based on name patterns
#'
#' Converts columns whose names match any of the supplied regex patterns
#' into factors. Optionally produces ordered factors.
#'
#' @param dataf A data frame.
#' @param patterns Character vector of regex patterns to match column names.
#' @param exclude Optional character vector of regex patterns. Columns whose
#'   names match any exclusion pattern are skipped, even if they match an
#'   inclusion pattern.
#' @param ordered If `TRUE`, create ordered factors. Default `FALSE`.
#' @param quiet If `TRUE`, suppress informational messages. Default `FALSE`.
#'
#' @return `dataf` with matching columns converted to (possibly ordered)
#'   factors.
#'
#' @examples
#' df <- data.frame(
#'   cdx_var1 = c("A", "B", "A", "C"),
#'   cdx_var2 = c("X", "Y", "X", "Z"),
#'   age      = c(25, 30, 22, 40),
#'   gender   = c("M", "M", "F", "M")
#' )
#' convert_columns_to_factors(df, patterns = c("cdx", "gender"))
#'
#' @export
convert_columns_to_factors <- function(dataf, patterns,
                                       exclude = NULL,
                                       ordered = FALSE,
                                       quiet   = FALSE) {

  validate_args(
    data     = dataf,
    patterns = is_nonempty_character(),
    ordered  = is_flag(),
    quiet    = is_flag(),
    custom_checks = list(
      list(
        condition = is.null(exclude) || is.character(exclude),
        message   = "{.arg exclude} must be NULL or a character vector"
      )
    )
  )

  matching_cols <- .find_cols_for_conversion(dataf, patterns, exclude)

  if (length(matching_cols) == 0L) {
    if (!quiet) {
      msg <- if (is.null(exclude)) {
        "No columns matching {.val {patterns}}"
      } else {
        "No columns matching {.val {patterns}} after excluding {.val {exclude}}"
      }
      cli::cli_alert_warning(msg)
    }
    return(dataf)
  }

  result <- dataf
  for (col in matching_cols) {
    result[[col]] <- .to_factor(dataf[[col]], ordered)
  }

  if (!quiet) {
    cli::cli_alert_success(
      "Converted {length(matching_cols)} column{?s} to factor{?s}: {.field {matching_cols}}"
    )
  }

  result
}

#' Find columns matching inclusion patterns minus exclusion patterns
#' (case-insensitive by default).
#' @keywords internal
#' @noRd
.find_cols_for_conversion <- function(dataf, patterns, exclude) {
  combined <- paste(patterns, collapse = "|")
  matching <- grep(combined, names(dataf), value = TRUE, ignore.case = TRUE)

  if (!is.null(exclude) && length(exclude) > 0L) {
    excl_pat <- paste(exclude, collapse = "|")
    matching <- matching[!grepl(excl_pat, matching, ignore.case = TRUE)]
  }
  matching
}

#' Convert a vector to factor, preserving levels when promoting to ordered.
#' @keywords internal
#' @noRd
.to_factor <- function(x, ordered) {
  if (is.factor(x)) {
    if (ordered && !is.ordered(x)) {
      # Use ordered() rather than factor(..., ordered = TRUE) so unobserved
      # levels are preserved.
      return(ordered(x, levels = levels(x)))
    }
    return(x)
  }
  factor(x, ordered = ordered)
}
