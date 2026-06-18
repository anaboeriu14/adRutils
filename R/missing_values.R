#' Summarize missing values by column
#'
#' Returns a tibble with one row per column of `data`, listing the count and
#' percentage of missing values, sorted from most to least missing.
#'
#' By default only `NA` values are counted. Use `na_strings` to also count
#' empty or sentinel strings (e.g. `""`, `"."`, `"NA"`) as missing. In that
#' case the output gains `count_blank`, `count_missing`, and `percent_missing`
#' columns, and `threshold` filters on `percent_missing`.
#'
#' @param data A data frame or tibble.
#' @param threshold Optional numeric in `[0, 100]`. If supplied, only columns
#'   at or above this missingness percentage are returned.
#' @param na_strings Optional character vector of string values to treat as
#'   missing, compared after trimming whitespace (so `""` also matches
#'   whitespace-only entries). Applied to character and factor columns only.
#'   When `NULL` (default), only true `NA` is counted.
#'
#' @return A tibble sorted from most to least missing. With `na_strings = NULL`:
#'   columns `column`, `count_na`, `percent_na`. With `na_strings` supplied:
#'   columns `column`, `count_na`, `count_blank`, `count_missing`,
#'   `percent_missing`. Percentages are rounded to 3 decimal places.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c("x", "", "  ", "y", NA),
#'   stringsAsFactors = FALSE
#' )
#' summarize_missingness(df)
#' summarize_missingness(df, na_strings = "")
#'
#' @seealso [drop_cols_by_missingness()] to remove high-missingness columns.
#' @export
summarize_missingness <- function(data, threshold = NULL, na_strings = NULL) {

  validate_args(
    data = data,
    custom_checks = list(
      list(
        condition = is.null(threshold) ||
          (is.numeric(threshold) && length(threshold) == 1L &&
             threshold >= 0 && threshold <= 100),
        message   = "{.arg threshold} must be NULL or a single number in [0, 100]"
      ),
      list(
        condition = is.null(na_strings) || is.character(na_strings),
        message   = "{.arg na_strings} must be NULL or a character vector"
      )
    )
  )

  n_rows    <- nrow(data)
  na_counts <- colSums(is.na(data))
  col_names <- names(na_counts)
  na_counts <- unname(na_counts)

  # Default path: true NA only (unchanged behavior).
  if (is.null(na_strings)) {
    result <- tibble::tibble(
      column     = col_names,
      count_na   = na_counts,
      percent_na = round((na_counts / n_rows) * 100, 3)
    ) %>%
      dplyr::arrange(dplyr::desc(.data$percent_na))

    if (!is.null(threshold)) {
      result <- dplyr::filter(result, .data$percent_na >= threshold)
    }
    return(result)
  }

  # Blank-aware path: count_na keeps its original meaning (true NA); blanks are
  # counted separately on character/factor columns and summed into count_missing.
  blank_counts <- unname(vapply(data, function(col) {
    if (is.character(col) || is.factor(col)) {
      sum(!is.na(col) & trimws(as.character(col)) %in% na_strings)
    } else {
      0
    }
  }, numeric(1)))

  missing_counts <- na_counts + blank_counts

  result <- tibble::tibble(
    column          = col_names,
    count_na        = na_counts,
    count_blank     = blank_counts,
    count_missing   = missing_counts,
    percent_missing = round((missing_counts / n_rows) * 100, 3)
  ) %>%
    dplyr::arrange(dplyr::desc(.data$percent_missing))

  if (!is.null(threshold)) {
    result <- dplyr::filter(result, .data$percent_missing >= threshold)
  }

  result
}


#' Drop columns with a high percentage of missing values
#'
#' Removes columns from `data` whose missingness percentage is at or above
#' `threshold`. To inspect what would be dropped without modifying the data,
#' call [summarize_missingness()] with the same `threshold` (and `na_strings`).
#'
#' @inheritParams summarize_missingness
#' @param threshold Numeric in `[0, 100]`. Columns at or above this
#'   missingness percentage are removed. Default `99`.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `data` with high-missingness columns removed.
#'
#' @examples
#' df <- data.frame(
#'   id               = 1:100,
#'   almost_empty     = c(rep(NA, 99), 1),
#'   completely_empty = rep(NA, 100),
#'   mostly_filled    = c(rep(NA, 20), 1:80)
#' )
#' drop_cols_by_missingness(df, threshold = 90)
#'
#' @seealso [summarize_missingness()] for inspecting missingness without
#'   removing columns.
#' @export
drop_cols_by_missingness <- function(data, threshold = 99,
                                     na_strings = NULL, quiet = FALSE) {

  validate_args(
    data  = data,
    quiet = is_flag(),
    custom_checks = list(
      list(
        condition = is.numeric(threshold) && length(threshold) == 1L &&
          threshold >= 0 && threshold <= 100,
        message   = "{.arg threshold} must be a single number in [0, 100]"
      ),
      list(
        condition = is.null(na_strings) || is.character(na_strings),
        message   = "{.arg na_strings} must be NULL or a character vector"
      )
    )
  )

  na_info        <- summarize_missingness(data, na_strings = na_strings)
  pct_col        <- if (is.null(na_strings)) "percent_na" else "percent_missing"
  cols_to_remove <- na_info$column[na_info[[pct_col]] >= threshold]

  if (length(cols_to_remove) == 0L) {
    if (!quiet) {
      cli::cli_alert_success("No columns with >= {threshold}% missing values")
    }
    return(data)
  }

  if (!quiet) {
    cli::cli_alert_info(
      "Removing {length(cols_to_remove)} column{?s} with >= {threshold}% missing"
    )
  }
  data[, !(names(data) %in% cols_to_remove), drop = FALSE]
}
