#' Remove duplicate rows based on ID, keeping the most complete row
#'
#' Identifies and removes duplicate rows based on a specified ID column.
#' Provides information about duplicate patterns and keeps the row with
#' the fewest NA values (most complete information) when duplicates exist.
#'
#' @param dataf A data frame to check for duplicates
#' @param id_col Column name (quoted or unquoted) specifying the ID to check for duplicates
#' @param keep One of "first", "last", "none", or "most_complete" (default: "most_complete").
#'   "most_complete" keeps the row with fewest NAs
#' @param quiet Logical. If TRUE, suppresses messages about duplicates (default: FALSE)
#'
#' @return A data frame with duplicates removed according to the 'keep' parameter
#'
#' @examples
#' \dontrun{
#' testDF <- data.frame(
#'   id_num = c(1, 1, 2, 2, 3, 4, 5, 5, 6),
#'   value = c(100, NA, 200, 250, 300, 400, 500, 500, 600),
#'   group = c("A", "A", "B", NA, "C", "D", "E", "F", "G")
#' )
#'
#' # Keep most complete row (default) - both quoted and unquoted work
#' remove_duplicates_if_exists(testDF, "id_num")
#' remove_duplicates_if_exists(testDF, id_num)
#'
#' # Keep first occurrence
#' remove_duplicates_if_exists(testDF, "id_num", keep = "first")
#'
#' # Silently remove duplicates
#' remove_duplicates_if_exists(testDF, "id_num", quiet = TRUE)
#' }
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

  # Identify duplicates
  dupe_info <- .identify_duplicates(id_values)

  # No duplicates found
  if (dupe_info$n_unique == 0) {
    if (!quiet) cli::cli_alert_info("No duplicates found")
    return(dataf)
  }

  # Report duplicate information
  if (!quiet) {
    .report_duplicate_info(dataf, id_values, dupe_info)
  }

  # Get ID column name for later use
  id_col_name <- .get_id_col_name({{ id_col }})

  # Remove duplicates based on strategy
  result <- .remove_duplicates_by_strategy(dataf, id_values, dupe_info, keep, id_col_name)

  # Report results
  if (!quiet) {
    n_removed <- nrow(dataf) - nrow(result)
    cli::cli_alert_success("Removed {n_removed} duplicate row{?s}")
  }

  return(result)
}

# Internal helper functions -----------------------------------------------

#' Extract and validate ID column using rlang
#' @keywords internal
.extract_id_column <- function(dataf, id_col) {
  # Capture the quoted expression
  col_quo <- rlang::enquo(id_col)

  # Convert to string
  col_name <- tryCatch({
    rlang::as_name(col_quo)
  }, error = function(e) {
    cli::cli_abort(c(
      "Invalid column specification",
      "i" = "Use a column name as a string: {.code \"med_id\"}",
      "i" = "Or unquoted: {.code med_id}"
    ))
  })

  # Validate column exists
  if (!col_name %in% names(dataf)) {
    cli::cli_abort("Column {.field {col_name}} not found in data")
  }

  # Return the column values
  return(dataf[[col_name]])
}

#' Get ID column name as string using rlang
#' @keywords internal
.get_id_col_name <- function(id_col) {
  col_quo <- rlang::enquo(id_col)
  rlang::as_name(col_quo)
}

#' Identify duplicate IDs and their unique values
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

#' Report information about duplicates
#' @keywords internal
.report_duplicate_info <- function(dataf, id_values, dupe_info) {
  cli::cli_alert_info("Found {dupe_info$n_unique} unique ID{?s} with duplicates")

  # Categorize duplicates
  categorized <- .categorize_duplicates(dataf, id_values, dupe_info$unique_ids)

  # Report identical duplicates
  if (length(categorized$identical) > 0) {
    preview <- .preview_list(categorized$identical, max_show = 10)
    cli::cli_alert_info("IDs with identical duplicate rows: {.val {preview}}")
  }

  # Report conflicting duplicates
  if (length(categorized$conflicting) > 0) {
    preview <- .preview_list(categorized$conflicting, max_show = 10)
    cli::cli_alert_warning("IDs with conflicting values across rows: {.val {preview}}")
  }

  invisible(NULL)
}

#' Categorize duplicates as identical or conflicting
#' @keywords internal
.categorize_duplicates <- function(dataf, id_values, unique_ids) {
  identical_ids <- character()
  conflicting_ids <- character()

  for (dup_id in unique_ids) {
    rows_with_id <- which(id_values == dup_id)
    rows_data <- dataf[rows_with_id, , drop = FALSE]

    if (.all_rows_identical(rows_data)) {
      identical_ids <- c(identical_ids, as.character(dup_id))
    } else {
      conflicting_ids <- c(conflicting_ids, as.character(dup_id))
    }
  }

  list(
    identical = identical_ids,
    conflicting = conflicting_ids
  )
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

#' Remove duplicates based on selected strategy
#' @keywords internal
.remove_duplicates_by_strategy <- function(dataf, id_values, dupe_info, keep, id_col_name) {
  result <- switch(keep,
                   first = .keep_first(dataf, id_values),
                   last = .keep_last(dataf, id_values),
                   none = .keep_none(dataf, dupe_info$mask),
                   most_complete = .keep_most_complete(dataf, id_values, dupe_info$unique_ids, id_col_name)
  )

  return(result)
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
.find_most_complete_row <- function(dataf, id_values, dup_id, id_col_name) {
  # Handle NA IDs separately
  if (is.na(dup_id)) {
    na_rows <- which(is.na(id_values))
    return(if (length(na_rows) > 0) na_rows[1] else NULL)
  }

  # Find rows with this ID
  rows_with_id <- which(id_values == dup_id & !is.na(id_values))

  if (length(rows_with_id) == 0) {
    return(NULL)
  }

  # Count NAs in each row (excluding ID column)
  na_counts <- .count_nas_excluding_id(dataf, rows_with_id, id_col_name)

  if (length(na_counts) == 0 || all(is.na(na_counts))) {
    return(rows_with_id[1])  # Fallback to first
  }

  # Find row with minimum NAs
  min_nas <- min(na_counts, na.rm = TRUE)
  best_rows <- rows_with_id[na_counts == min_nas & !is.na(na_counts)]

  if (length(best_rows) > 0) {
    return(best_rows[1])
  } else {
    return(rows_with_id[1])
  }
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
