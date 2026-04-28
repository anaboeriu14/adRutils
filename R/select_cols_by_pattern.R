#' Select columns by regex pattern
#'
#' Selects columns from a data frame whose names match any of the supplied
#' regex patterns, with optional exclusion patterns and an `invert` flag
#' to return columns that do *not* match.
#'
#' @details
#' Multiple patterns are combined with OR logic. Exclusion patterns are
#' applied after initial matching. When `invert = TRUE`, the function
#' returns columns that do *not* match any inclusion pattern (after
#' applying exclusions). Matching is case-insensitive by default.
#'
#' @param dataf A data frame or tibble.
#' @param patterns Character vector of regex patterns to match column
#'   names.
#' @param exclude Optional character vector of regex patterns. Matching
#'   columns are dropped after inclusion is applied.
#' @param ignore_case If `TRUE` (default), perform case-insensitive matching.
#' @param warn_no_match If `TRUE` (default), emit a warning and return an
#'   empty data frame when no columns match. If `FALSE`, abort.
#' @param invert If `TRUE`, return columns that do NOT match the patterns.
#'   Default `FALSE`.
#'
#' @return A data frame containing the selected columns. When no columns
#'   match and `warn_no_match = TRUE`, a 0-column data frame is returned.
#'
#' @examples
#' df <- data.frame(
#'   id         = 1:5,
#'   name_first = c("John", "Jane", "Bob", "Alice", "Tom"),
#'   name_last  = c("Smith", "Doe", "Johnson", "Brown", "Wilson"),
#'   age        = c(25, 30, 35, 40, 45),
#'   score_math = c(85, 90, 78, 92, 88),
#'   score_eng  = c(76, 94, 82, 88, 79)
#' )
#'
#' select_cols_by_pattern(df, "name")
#' select_cols_by_pattern(df, "score", exclude = "math")
#' select_cols_by_pattern(df, "score", invert = TRUE)
#' select_cols_by_pattern(df, c("id", "age"))
#'
#' @export
select_cols_by_pattern <- function(dataf,
                                   patterns,
                                   exclude       = NULL,
                                   ignore_case   = TRUE,
                                   warn_no_match = TRUE,
                                   invert        = FALSE) {

  validate_args(
    data          = dataf,
    patterns      = is_nonempty_character(),
    ignore_case   = is_flag(),
    warn_no_match = is_flag(),
    invert        = is_flag(),
    custom_checks = list(
      list(
        condition = is.null(exclude) || is.character(exclude),
        message   = "{.arg exclude} must be NULL or a character vector"
      )
    )
  )

  matched_cols <- .find_matching_columns(dataf, patterns, exclude, ignore_case)

  if (length(matched_cols) == 0L) {
    return(.handle_no_matches(dataf, patterns, exclude, warn_no_match))
  }

  if (invert) {
    return(dataf[, !(names(dataf) %in% matched_cols), drop = FALSE])
  }
  dataf[, matched_cols, drop = FALSE]
}


#' Match column names against patterns and apply exclusions.
#' @keywords internal
#' @noRd
.find_matching_columns <- function(dataf, patterns, exclude, ignore_case) {
  combined <- paste(patterns, collapse = "|")
  if (ignore_case) combined <- paste0("(?i)", combined)

  matches <- grep(combined, names(dataf), value = TRUE, perl = TRUE)

  if (!is.null(exclude) && length(exclude) > 0L) {
    excl <- paste(exclude, collapse = "|")
    if (ignore_case) excl <- paste0("(?i)", excl)
    matches <- matches[!grepl(excl, matches, perl = TRUE)]
  }
  matches
}


#' Either warn (returning an empty data frame) or abort, per user choice.
#' @keywords internal
#' @noRd
.handle_no_matches <- function(dataf, patterns, exclude, warn_no_match) {
  msg <- paste0(
    "No columns matched pattern(s): ",
    paste(patterns, collapse = ", ")
  )
  if (!is.null(exclude)) {
    msg <- paste0(msg, " after excluding: ", paste(exclude, collapse = ", "))
  }

  if (warn_no_match) {
    cli::cli_alert_warning(msg)
    return(dataf[, integer(0), drop = FALSE])
  }
  cli::cli_abort(msg)
}
