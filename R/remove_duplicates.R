#' Remove duplicate rows by ID
#'
#' Identifies and removes duplicate rows based on a single ID column. By
#' default, keeps the row with the fewest `NA` values per duplicate ID
#' (the "most complete" row).
#'
#' @param dataf A data frame.
#' @param id_col Column name (quoted or unquoted) identifying rows.
#' @param keep One of `"most_complete"` (default), `"first"`, `"last"`, or
#'   `"none"`. `"none"` removes *all* rows with duplicate IDs, including
#'   their originals.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `dataf` with duplicates removed according to `keep`.
#'
#' @examples
#' df <- data.frame(
#'   id  = c(1, 2, 2, 3),
#'   val = c(10, NA, 20, 30)
#' )
#' remove_duplicates(df, id_col = "id")  # keeps id=2 row with val=20
#'
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
    dataf, id_values, dupe_info, keep, id_col_name
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

  ids       <- as.character(dupe_info$unique_ids)
  preview   <- if (length(ids) <= 5L) ids else c(utils::head(ids, 5L), "...")
  cli::cli_alert_info("Duplicate IDs: {.val {preview}}")
}


#' Apply the requested deduplication strategy.
#' @keywords internal
#' @noRd
.remove_duplicates_by_strategy <- function(dataf, id_values, dupe_info,
                                           keep, id_col_name) {
  switch(keep,
         first         = dataf[!duplicated(id_values), ],
         last          = dataf[!duplicated(id_values, fromLast = TRUE), ],
         none          = dataf[!dupe_info$mask, ],
         most_complete = .keep_most_complete(dataf, id_values,
                                             dupe_info$unique_ids, id_col_name)
  )
}


#' For each duplicate ID, keep only the row with the fewest NAs.
#' @keywords internal
#' @noRd
.keep_most_complete <- function(dataf, id_values, unique_ids, id_col_name) {
  rows_to_keep <- logical(nrow(dataf))

  non_dupes <- !(duplicated(id_values) | duplicated(id_values, fromLast = TRUE))
  rows_to_keep[non_dupes] <- TRUE

  for (dup_id in unique_ids) {
    best_row <- .find_most_complete_row(dataf, id_values, dup_id, id_col_name)
    if (!is.null(best_row)) rows_to_keep[best_row] <- TRUE
  }

  dataf[rows_to_keep, ]
}


#' Locate the row index with fewest NAs (excluding the ID column) for a
#' given duplicated ID.
#' @keywords internal
#' @noRd
.find_most_complete_row <- function(dataf, id_values, dup_id, id_col_name) {
  rows_with_id <- if (is.na(dup_id)) {
    which(is.na(id_values))
  } else {
    which(id_values == dup_id & !is.na(id_values))
  }

  if (length(rows_with_id) == 0L) return(NULL)
  if (length(rows_with_id) == 1L) return(rows_with_id)

  na_counts <- .count_nas_excluding_id(dataf, rows_with_id, id_col_name)
  rows_with_id[which.min(na_counts)]
}


#' Count NAs in a set of rows, excluding the ID column.
#' @keywords internal
#' @noRd
.count_nas_excluding_id <- function(dataf, row_indices, id_col_name) {
  vapply(row_indices, function(row_idx) {
    row_data   <- dataf[row_idx, , drop = FALSE]
    id_col_idx <- which(names(dataf) == id_col_name)
    if (length(id_col_idx) > 0L) {
      row_data <- row_data[, -id_col_idx, drop = FALSE]
    }
    sum(is.na(row_data))
  }, FUN.VALUE = integer(1))
}
