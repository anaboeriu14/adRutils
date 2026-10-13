#' Remove duplicate rows by ID
#'
#' Identifies rows that share an ID and keeps one row per ID according to
#' `keep`. By default, keeps the row with the fewest `NA` values (the
#' "most complete" row).
#'
#' @details
#' With `keep = "most_complete"`, each duplicated ID is handled as follows:
#'
#' * **Identical rows**: the first occurrence is kept.
#' * **Rows that differ only in missingness**: the row with the fewest
#'   `NA` values is kept.
#' * **Rows with conflicting non-missing values** (e.g., two different
#'   ages for the same ID): a warning lists the affected IDs, and the row
#'   with the fewest `NA` values is kept. Ties are broken by row order.
#'
#' Rows are never merged; the kept row always exists in the input as-is.
#' Values are compared with exact equality, so numeric values differing
#' only by floating-point noise are treated as conflicting.
#'
#' @param dataf A data frame.
#' @param id_col Column name (quoted or unquoted) identifying rows.
#' @param keep One of `"most_complete"` (default), `"first"`, `"last"`, or
#'   `"none"`. `"none"` removes *all* rows with duplicate IDs, including
#'   their originals.
#' @param quiet If `TRUE`, suppress messages. Conflict warnings are still
#'   recorded in the `"conflicting_ids"` attribute. Default `FALSE`.
#'
#' @return `dataf` with duplicates removed according to `keep`. When
#'   `keep = "most_complete"`, the IDs with conflicting values are attached
#'   as an attribute; access them via `attr(result, "conflicting_ids")`.
#'
#' @examples
#' df <- data.frame(
#'   id  = c(1, 2, 2, 3, 3, 4, 4),
#'   age = c(60, NA, 65, 70, 70, 50, 55),
#'   bmi = c(22, 24, 24, 30, 30, 21, NA)
#' )
#' # id 2: keeps the more complete row
#' # id 3: identical rows, keeps the first
#' # id 4: conflicting ages, warns and keeps the more complete row
#' result <- remove_duplicates(df, id_col = "id")
#' attr(result, "conflicting_ids")
#'
#' @seealso [dplyr::distinct()] for removing rows that are identical across
#'   all columns regardless of ID.
#' @export
remove_duplicates <- function(dataf, id_col,
                              keep  = c("most_complete", "first", "last", "none"),
                              quiet = FALSE) {

  keep <- match.arg(keep)

  validate_args(
    data  = dataf,
    quiet = is_flag()
  )

  id_values   <- .extract_id_column(dataf, {{ id_col }})
  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  dupe_info <- .identify_duplicates(id_values)

  if (dupe_info$n_unique == 0L) {
    if (!quiet) cli::cli_alert_info("No duplicates found")
    return(dataf)
  }

  if (!quiet) .report_duplicate_info(dupe_info)

  result <- .remove_duplicates_by_strategy(
    dataf, id_values, dupe_info, keep, id_col_name, quiet
  )

  if (!quiet) {
    n_removed <- nrow(dataf) - nrow(result)
    cli::cli_alert_success("Removed {n_removed} duplicate row{?s}")
  }

  result
}


#' Extract and validate the ID column using rlang quasiquotation.
#' @keywords internal
#' @noRd
.extract_id_column <- function(dataf, id_col) {
  col_name <- rlang::as_name(rlang::enquo(id_col))
  if (!col_name %in% names(dataf)) {
    cli::cli_abort("Column {.field {col_name}} not found in data")
  }
  dataf[[col_name]]
}


#' Identify duplicate IDs.
#' @keywords internal
#' @noRd
.identify_duplicates <- function(id_values) {
  dupe_mask  <- duplicated(id_values) | duplicated(id_values, fromLast = TRUE)
  unique_ids <- unique(id_values[dupe_mask])

  list(
    mask       = dupe_mask,
    unique_ids = unique_ids,
    n_unique   = length(unique_ids)
  )
}


#' Report which IDs are duplicated.
#' @keywords internal
#' @noRd
.report_duplicate_info <- function(dupe_info) {
  cli::cli_alert_info("Found {dupe_info$n_unique} unique ID{?s} with duplicates")

  ids     <- as.character(dupe_info$unique_ids)
  preview <- if (length(ids) <= 5L) ids else c(utils::head(ids, 5L), "...")
  cli::cli_alert_info("Duplicate IDs: {.val {preview}}")
}


#' Apply the requested deduplication strategy.
#' @keywords internal
#' @noRd
.remove_duplicates_by_strategy <- function(dataf, id_values, dupe_info,
                                           keep, id_col_name, quiet) {
  switch(keep,
         first         = dataf[!duplicated(id_values), , drop = FALSE],
         last          = dataf[!duplicated(id_values, fromLast = TRUE), , drop = FALSE],
         none          = dataf[!dupe_info$mask, , drop = FALSE],
         most_complete = .keep_most_complete(dataf, id_values, dupe_info,
                                             id_col_name, quiet)
  )
}


#' For each duplicate ID, keep the row with the fewest NAs (first on ties).
#' Warns about IDs whose rows have conflicting non-missing values.
#' @keywords internal
#' @noRd
.keep_most_complete <- function(dataf, id_values, dupe_info,
                                id_col_name, quiet) {
  rows_to_keep <- !dupe_info$mask
  value_cols   <- setdiff(names(dataf), id_col_name)
  conflicting  <- character()

  for (dup_id in dupe_info$unique_ids) {
    idx <- if (is.na(dup_id)) {
      which(is.na(id_values))
    } else {
      which(id_values == dup_id)
    }
    rows <- dataf[idx, value_cols, drop = FALSE]

    if (.has_conflicting_values(rows)) {
      conflicting <- c(conflicting, as.character(dup_id))
    }

    na_counts <- rowSums(is.na(rows))
    rows_to_keep[idx[which.min(na_counts)]] <- TRUE
  }

  if (!quiet && length(conflicting) > 0L) {
    cli::cli_alert_warning(
      "{length(conflicting)} ID{?s} with conflicting values (kept most complete row): {.val {conflicting}}"
    )
  }

  result <- dataf[rows_to_keep, , drop = FALSE]
  attr(result, "conflicting_ids") <- conflicting
  result
}


#' TRUE if any column has more than one distinct non-missing value
#' across the rows sharing an ID.
#' @keywords internal
#' @noRd
.has_conflicting_values <- function(rows) {
  any(vapply(rows, function(x) {
    length(unique(x[!is.na(x)])) > 1L
  }, logical(1)))
}
