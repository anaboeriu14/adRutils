#' Create an ID mapping table from a dataset
#'
#' Extracts the unique pairs of two ID columns from a dataset, producing a
#' lookup table suitable for use with [add_id_mapping()].
#'
#' @param data A data frame containing both ID columns.
#' @param id_col1,id_col2 Names of the two ID columns. Must differ.
#' @param trim If `TRUE` (default), trim leading/trailing whitespace from
#'   the IDs before deduplication.
#'
#' @return A data frame with two columns of unique ID pairs.
#'
#' @examples
#' df <- data.frame(
#'   short_id = c("A1", "A2", "A1"),
#'   long_id  = c("subj_001", "subj_002", "subj_001")
#' )
#' create_id_mapping(df, "short_id", "long_id")
#'
#' @export
create_id_mapping <- function(data, id_col1, id_col2, trim = TRUE) {

  validate_args(
    data    = data,
    columns = c(id_col1, id_col2),
    id_col1 = is_string(),
    id_col2 = is_string(),
    trim    = is_flag(),
    custom_checks = list(
      list(
        condition = id_col1 != id_col2,
        message   = "{.arg id_col1} and {.arg id_col2} must differ"
      )
    )
  )

  mapping <- data %>%
    dplyr::select(dplyr::all_of(c(id_col1, id_col2))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), .normalize_id, trim = trim))

  .check_mapping_duplicates(mapping, id_col1)
  .check_mapping_duplicates(mapping, id_col2)

  cli::cli_alert_success("Created mapping with {nrow(mapping)} unique ID pair{?s}")
  mapping
}


#' Add an ID column via lookup
#'
#' Joins an ID mapping table to `data`, adding `id_col_to_add` based on
#' matches in `existing_id_col`. If `id_col_to_add` already exists in
#' `data`, it is replaced.
#'
#' @param data A data frame.
#' @param id_mapping A lookup table containing both `existing_id_col` and
#'   `id_col_to_add`.
#' @param existing_id_col Name of the ID column already in `data`.
#' @param id_col_to_add Name of the ID column to add from `id_mapping`.
#' @param trim If `TRUE` (default), trim whitespace before matching.
#' @param quiet If `TRUE`, suppress matching statistics. Default `FALSE`.
#'
#' @return `data` with `id_col_to_add` added (or replaced).
#'
#' @export
add_id_mapping <- function(data, id_mapping,
                           existing_id_col,
                           id_col_to_add,
                           trim  = TRUE,
                           quiet = FALSE) {

  validate_args(
    data            = data,
    columns         = existing_id_col,
    existing_id_col = is_string(),
    id_col_to_add   = is_string(),
    trim            = is_flag(),
    quiet           = is_flag(),
    custom_checks = list(
      list(
        condition = is.data.frame(id_mapping),
        message   = "{.arg id_mapping} must be a data frame"
      ),
      list(
        condition = existing_id_col %in% names(id_mapping) &&
          id_col_to_add %in% names(id_mapping),
        message   = paste0(
          "{.arg id_mapping} must contain both {.field {existing_id_col}} ",
          "and {.field {id_col_to_add}}"
        )
      ),
      list(
        condition = existing_id_col != id_col_to_add,
        message   = "{.arg existing_id_col} and {.arg id_col_to_add} must differ"
      )
    )
  )

  if (!quiet && id_col_to_add %in% names(data)) {
    cli::cli_alert_warning("Overwriting existing column: {.field {id_col_to_add}}")
  }

  mapping <- id_mapping %>%
    dplyr::select(dplyr::all_of(c(existing_id_col, id_col_to_add))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), .normalize_id, trim = trim))

  if (!quiet) .check_mapping_duplicates(mapping, existing_id_col)

  # Drop existing target column (if present) so the join doesn't suffix.
  if (id_col_to_add %in% names(data)) {
    data <- data[, setdiff(names(data), id_col_to_add), drop = FALSE]
  }

  prepared <- data %>%
    dplyr::mutate(!!existing_id_col := .normalize_id(.data[[existing_id_col]],
                                                     trim = trim))

  result <- dplyr::left_join(prepared, mapping, by = existing_id_col)

  if (!quiet) .report_id_matching(result, id_col_to_add)
  result
}


#' Coerce an ID vector to character, optionally trimming whitespace.
#' @keywords internal
#' @noRd
.normalize_id <- function(x, trim = TRUE) {
  out <- as.character(x)
  if (trim) out <- stringr::str_trim(out)
  out
}

#' Warn if a column has duplicates (one-to-many mapping).
#' @keywords internal
#' @noRd
.check_mapping_duplicates <- function(mapping, id_col) {
  n_dupes <- sum(duplicated(mapping[[id_col]]))
  if (n_dupes > 0L) {
    cli::cli_alert_warning(
      "{n_dupes} duplicate{?s} in {.field {id_col}} - one-to-many mapping"
    )
  }
  invisible(NULL)
}

#' Report how many rows in `data` matched after the join.
#' @keywords internal
#' @noRd
.report_id_matching <- function(data, id_col) {
  n_matched <- sum(!is.na(data[[id_col]]))
  n_total   <- nrow(data)

  if (n_matched < n_total) {
    n_unmatched  <- n_total - n_matched
    pct          <- round((n_unmatched / n_total) * 100, 1)
    cli::cli_alert_warning("{n_unmatched} row{?s} ({pct}%) unmatched")
  } else {
    cli::cli_alert_success("All {n_total} row{?s} matched")
  }
  invisible(NULL)
}
