#' Remove duplicate rows based on ID, keeping the most complete row
#'
#' Identifies and removes duplicate rows based on a specified ID column.
#' Keeps the row with the fewest NA values (most complete) when duplicates exist.
#'
#' @param dataf A data frame to check for duplicates
#' @param id_col Column name (quoted or unquoted) specifying the ID
#' @param keep One of "first", "last", "none", or "most_complete" (default: "most_complete")
#' @param quiet Logical. If TRUE, suppresses messages (default: FALSE)
#'
#' @return A data frame with duplicates removed
#'
#' @export
remove_duplicates_if_exists <- function(dataf, id_col,
                                        keep = c("most_complete", "first", "last", "none"),
                                        quiet = FALSE) {
  # Validate and match keep argument
  keep <- match.arg(keep)

  # Validate inputs
  validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = is.logical(quiet) && length(quiet) == 1,
        message = "{.arg quiet} must be a single logical value"
      )
    ),
    context = "remove_duplicates_if_exists"
  )

  # Extract and validate ID column
  id_values <- .extract_id_column(dataf, {{ id_col }})

  # Get ID column name
  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  # Identify duplicates
  dupe_info <- .identify_duplicates(id_values)

  # No duplicates found
  if (dupe_info$n_unique == 0) {
    if (!quiet) cli::cli_alert_info("No duplicates found")
    return(dataf)
  }

  # Report duplicate information
  if (!quiet) {
    .report_duplicate_info(dupe_info)
  }

  # Remove duplicates based on strategy
  result <- .remove_duplicates_by_strategy(dataf, id_values, dupe_info, keep, id_col_name)

  # Report results
  if (!quiet) {
    n_removed <- nrow(dataf) - nrow(result)
    cli::cli_alert_success("Removed {n_removed} duplicate row{?s}")
  }

  return(result)
}

#' Extract and validate ID column using rlang
#' @keywords internal
#' @noRd
.extract_id_column <- function(dataf, id_col) {
  col_name <- rlang::as_name(rlang::enquo(id_col))

  if (!col_name %in% names(dataf)) {
    cli::cli_abort("Column {.field {col_name}} not found in data")
  }

  return(dataf[[col_name]])
}

#' Identify duplicate IDs and their unique values
#' @keywords internal
#' @noRd
.identify_duplicates <- function(id_values) {
  dupe_mask <- duplicated(id_values) | duplicated(id_values, fromLast = TRUE)
  unique_ids <- unique(id_values[dupe_mask])

  list(
    mask = dupe_mask,
    unique_ids = unique_ids,
    n_unique = length(unique_ids)
  )
}

#' Report information about duplicates
#' @keywords internal
#' @noRd
.report_duplicate_info <- function(dupe_info) {
  cli::cli_alert_info("Found {dupe_info$n_unique} unique ID{?s} with duplicates")

  # Show which IDs have duplicates
  preview <- .preview_list(as.character(dupe_info$unique_ids), max_show = 10)
  cli::cli_alert_info("Duplicate IDs: {.val {preview}}")
}

#' Preview list with ellipsis for long lists
#' @keywords internal
#' @noRd
.preview_list <- function(items, max_show = 10) {
  if (length(items) <= max_show) {
    return(items)
  }
  c(head(items, max_show), "...")
}

#' Remove duplicates based on selected strategy
#' @keywords internal
#' @noRd
.remove_duplicates_by_strategy <- function(dataf, id_values, dupe_info, keep, id_col_name) {
  switch(keep,
         first = dataf[!duplicated(id_values), ],
         last = dataf[!duplicated(id_values, fromLast = TRUE), ],
         none = dataf[!dupe_info$mask, ],
         most_complete = .keep_most_complete(dataf, id_values, dupe_info$unique_ids, id_col_name)
  )
}

#' Keep most complete row (fewest NAs) for each ID
#' @keywords internal
#' @noRd
.keep_most_complete <- function(dataf, id_values, unique_ids, id_col_name) {
  rows_to_keep <- logical(nrow(dataf))

  # Keep all non-duplicate rows
  non_dupes <- !(duplicated(id_values) | duplicated(id_values, fromLast = TRUE))
  rows_to_keep[non_dupes] <- TRUE

  # For each duplicate ID, find the most complete row
  for (dup_id in unique_ids) {
    best_row <- .find_most_complete_row(dataf, id_values, dup_id, id_col_name)
    if (!is.null(best_row)) {
      rows_to_keep[best_row] <- TRUE
    }
  }

  return(dataf[rows_to_keep, ])
}

#' Find row with fewest NAs for a given ID
#' @keywords internal
#' @noRd
.find_most_complete_row <- function(dataf, id_values, dup_id, id_col_name) {
  # Find rows with this ID (handle NA IDs)
  rows_with_id <- if (is.na(dup_id)) {
    which(is.na(id_values))
  } else {
    which(id_values == dup_id & !is.na(id_values))
  }

  if (length(rows_with_id) == 0) return(NULL)
  if (length(rows_with_id) == 1) return(rows_with_id)  # No need to count NAs

  # Count NAs and return row with minimum
  na_counts <- .count_nas_excluding_id(dataf, rows_with_id, id_col_name)
  rows_with_id[which.min(na_counts)]
}

#' Count NAs in rows excluding ID column
#' @keywords internal
#' @noRd
.count_nas_excluding_id <- function(dataf, row_indices, id_col_name) {
  vapply(row_indices, function(row_idx) {
    row_data <- dataf[row_idx, , drop = FALSE]

    # Exclude ID column from NA counting
    id_col_idx <- which(names(dataf) == id_col_name)
    if (length(id_col_idx) > 0) {
      row_data <- row_data[, -id_col_idx, drop = FALSE]
    }

    sum(is.na(row_data))
  }, FUN.VALUE = integer(1))
}
