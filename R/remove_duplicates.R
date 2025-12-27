#' Remove duplicate rows based on ID, keeping the most complete row
#'
#' Identifies and removes duplicate rows based on a specified ID column.
#' Provides information about duplicate patterns and keeps the row with
#' the fewest NA values (most complete information) when duplicates exist.
#'
#' @param dataf A data frame to check for duplicates
#' @param id_col Column name or expression specifying the ID to check for duplicates
#' @param keep One of "first", "last", "none", or "most_complete" (default: "first").
#'        "most_complete" keeps the row with fewest NAs.
#' @param quiet Logical; if TRUE, suppresses messages about duplicate IDs
#'
#' @return A data frame with duplicates removed according to the 'keep' parameter
#'
#' @examples
#' testDF <- data.frame(
#'   id_num = c(1, 1, 2, 2, 3, 4, 5, 5, 6),
#'   value = c(100, NA, 200, 250, 300, 400, 500, 500, 600),
#'   group = c("A", "A", "B", NA, "C", "D", "E", "F", "G")
#' )
#' remove_duplicates_if_exists(testDF, "id_num", "most_complete")
#'
#' @export
remove_duplicates_if_exists <- function(dataf, id_col,
                                        keep = c("first", "last", "none", "most_complete"),
                                        quiet = FALSE) {

  # Validate and extract ID values
  id_values <- .extract_id_column(dataf, id_col)
  keep <- match.arg(keep)

  # Check for duplicates
  dupe_info <- .identify_duplicates(id_values)

  if (dupe_info$n_unique == 0) {
    if (!quiet) cli::cli_alert_info("No duplicates found")
    return(dataf)
  }

  # Analyze and report duplicate patterns
  if (!quiet) {
    .analyze_duplicate_patterns(dataf, id_values, dupe_info$unique_ids)
  }

  # Remove duplicates based on strategy
  result <- .remove_duplicates(dataf, id_values, dupe_info, keep, id_col)

  if (!quiet) {
    n_removed <- nrow(dataf) - nrow(result)
    cli::cli_alert_success("Removed {n_removed} duplicate row{?s}")
  }

  return(result)
}

#' Extract and validate ID column
#' @keywords internal
.extract_id_column <- function(dataf, id_col) {
  validate_params(
    data = dataf,
    context = "remove_duplicates_if_exists"
  )

  id_expr <- substitute(id_col)

  # Handle string or NSE
  if (is.character(id_expr)) {
    if (!id_expr %in% names(dataf)) {
      cli::cli_abort("Column {.field {id_expr}} not found in data")
    }
    return(dataf[[id_expr]])
  }

  # Handle NSE (non-standard evaluation)
  tryCatch(
    eval(id_expr, dataf, parent.frame()),
    error = function(e) {
      cli::cli_abort("Could not evaluate ID expression: {.code {deparse(id_expr)}}")
    }
  )
}

#' Identify duplicate IDs
#' @keywords internal
.identify_duplicates <- function(id_values) {
  dupe_mask <- duplicated(id_values) | duplicated(id_values, fromLast = TRUE)
  unique_ids <- unique(id_values[dupe_mask])

  list(
    mask = dupe_mask,
    unique_ids = unique_ids,
    n_unique = length(unique_ids)
  )
}

#' Analyze duplicate patterns (identical vs conflicting)
#' @keywords internal
.analyze_duplicate_patterns <- function(dataf, id_values, unique_ids) {
  cli::cli_alert_info("Found {length(unique_ids)} unique ID{?s} with duplicates")

  identical_ids <- character()
  conflicting_ids <- character()

  for (dup_id in unique_ids) {
    rows_with_id <- which(id_values == dup_id)
    rows_data <- dataf[rows_with_id, , drop = FALSE]

    # Check if all rows are identical
    if (.all_rows_identical(rows_data)) {
      identical_ids <- c(identical_ids, as.character(dup_id))
    } else {
      conflicting_ids <- c(conflicting_ids, as.character(dup_id))
    }
  }

  # Report findings
  if (length(identical_ids) > 0) {
    preview <- .preview_list(identical_ids, max_show = 10)
    cli::cli_alert_info("IDs with identical duplicate rows: {.val {preview}}")
  }

  if (length(conflicting_ids) > 0) {
    preview <- .preview_list(conflicting_ids, max_show = 10)
    cli::cli_alert_warning("IDs with conflicting values across rows: {.val {preview}}")
  }

  invisible(NULL)
}

#' Check if all rows in a dataframe are identical
#' @keywords internal
.all_rows_identical <- function(rows_data) {
  if (nrow(rows_data) <= 1) return(TRUE)

  first_row <- as.list(rows_data[1, , drop = FALSE])
  for (i in 2:nrow(rows_data)) {
    if (!identical(first_row, as.list(rows_data[i, , drop = FALSE]))) {
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Preview list with ellipsis for long lists
#' @keywords internal
.preview_list <- function(items, max_show = 10) {
  if (length(items) <= max_show) {
    return(items)
  }
  c(head(items, max_show), "...")
}

#' Remove duplicates based on keep strategy
#' @keywords internal
.remove_duplicates <- function(dataf, id_values, dupe_info, keep, id_col) {
  switch(keep,
         first = .keep_first(dataf, id_values),
         last = .keep_last(dataf, id_values),
         none = .keep_none(dataf, dupe_info$mask),
         most_complete = .keep_most_complete(dataf, id_values, dupe_info$unique_ids, id_col)
  )
}

#' Keep first occurrence of each ID
#' @keywords internal
.keep_first <- function(dataf, id_values) {
  dataf[!duplicated(id_values), ]
}

#' Keep last occurrence of each ID
#' @keywords internal
.keep_last <- function(dataf, id_values) {
  dataf[!duplicated(id_values, fromLast = TRUE), ]
}

#' Remove all rows with duplicate IDs
#' @keywords internal
.keep_none <- function(dataf, dupe_mask) {
  dataf[!dupe_mask, ]
}

#' Keep most complete row (fewest NAs) for each ID
#' @keywords internal
.keep_most_complete <- function(dataf, id_values, unique_ids, id_col) {
  rows_to_keep <- logical(nrow(dataf))

  # Keep all non-duplicate rows
  non_dupes <- !(duplicated(id_values) | duplicated(id_values, fromLast = TRUE))
  rows_to_keep[non_dupes] <- TRUE

  # For each duplicate ID, find the most complete row
  id_col_name <- .get_id_col_name(id_col)

  for (dup_id in unique_ids) {
    best_row <- .find_most_complete_row(dataf, id_values, dup_id, id_col_name)
    if (!is.null(best_row)) {
      rows_to_keep[best_row] <- TRUE
    }
  }

  return(dataf[rows_to_keep, ])
}

#' Get ID column name as string
#' @keywords internal
.get_id_col_name <- function(id_col) {
  id_expr <- substitute(id_col)
  if (is.character(id_expr)) {
    return(id_expr)
  }
  deparse(id_expr)
}

#' Find row with fewest NAs for a given ID
#' @keywords internal
.find_most_complete_row <- function(dataf, id_values, dup_id, id_col_name) {
  # Handle NA IDs separately
  if (is.na(dup_id)) {
    na_rows <- which(is.na(id_values))
    return(if (length(na_rows) > 0) na_rows[1] else NULL)
  }

  rows_with_id <- which(id_values == dup_id & !is.na(id_values))
  if (length(rows_with_id) == 0) return(NULL)

  # Count NAs in each row (excluding ID column)
  na_counts <- .count_nas_excluding_id(dataf, rows_with_id, id_col_name)

  if (length(na_counts) == 0 || all(is.na(na_counts))) {
    return(rows_with_id[1])  # Fallback to first
  }

  # Find row with minimum NAs
  min_nas <- min(na_counts, na.rm = TRUE)
  best_rows <- rows_with_id[na_counts == min_nas & !is.na(na_counts)]

  if (length(best_rows) > 0) best_rows[1] else rows_with_id[1]
}

#' Count NAs in rows excluding ID column
#' @keywords internal
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
