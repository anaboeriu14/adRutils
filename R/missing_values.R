#' Summarize NA values by column
#'
#' Returns a tibble with one row per column of `data`, listing the count
#' and percentage of `NA` values, sorted from most to least missing.
#'
#' @param data A data frame or tibble.
#' @param threshold Optional numeric in `[0, 100]`. If supplied, only
#'   columns with `percent_na >= threshold` are returned.
#'
#' @return A tibble with columns `column`, `count_na`, and `percent_na`
#'   (rounded to 3 decimal places).
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c(NA, NA, 3, 4, 5),
#'   c = 1:5
#' )
#' summarize_na(df)
#' summarize_na(df, threshold = 50)
#'
#' @export
summarize_na <- function(data, threshold = NULL) {

  validate_args(
    data = data,
    custom_checks = list(
      list(
        condition = is.null(threshold) ||
          (is.numeric(threshold) && length(threshold) == 1L &&
             threshold >= 0 && threshold <= 100),
        message   = "{.arg threshold} must be NULL or a single number in [0, 100]"
      )
    )
  )

  na_counts  <- colSums(is.na(data))
  na_percent <- round((na_counts / nrow(data)) * 100, 3)

  result <- tibble::tibble(
    column     = names(na_percent),
    count_na   = na_counts,
    percent_na = na_percent
  ) %>%
    dplyr::arrange(dplyr::desc(.data$percent_na))

  if (!is.null(threshold)) {
    result <- dplyr::filter(result, .data$percent_na >= threshold)
  }

  result
}


#' Drop columns with a high percentage of NA values
#'
#' Removes columns from `data` whose `NA` percentage is at or above
#' `threshold`. To inspect what would be dropped without modifying the
#' data, call [summarize_na()] with the same threshold.
#'
#' @param data A data frame or tibble.
#' @param threshold Numeric in `[0, 100]`. Columns with
#'   `percent_na >= threshold` are removed. Default `99`.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `data` with high-NA columns removed.
#'
#' @examples
#' df <- data.frame(
#'   id              = 1:100,
#'   almost_empty    = c(rep(NA, 99), 1),
#'   completely_empty = rep(NA, 100),
#'   mostly_filled   = c(rep(NA, 20), 1:80)
#' )
#' drop_high_na_cols(df, threshold = 90)
#'
#' @seealso [summarize_na()] for inspecting NA patterns without removing
#'   columns.
#' @export
drop_high_na_cols <- function(data, threshold = 99, quiet = FALSE) {

  validate_args(
    data  = data,
    quiet = is_flag(),
    custom_checks = list(
      list(
        condition = is.numeric(threshold) && length(threshold) == 1L &&
          threshold >= 0 && threshold <= 100,
        message   = "{.arg threshold} must be a single number in [0, 100]"
      )
    )
  )

  na_info        <- summarize_na(data)
  cols_to_remove <- na_info$column[na_info$percent_na >= threshold]

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
