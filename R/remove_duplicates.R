#' Remove duplicate rows based on ID
#'
#' This function identifies and removes duplicate rows based on a specified ID column.
#' It provides information about which IDs have duplicates and whether the duplicate
#' rows are identical or have conflicting values.
#'
#' @param dataf A data frame to check for duplicates
#' @param id_col Column name or expression specifying the ID to check for duplicates
#' @param keep One of "first" (default), "last", or "none". Determines which occurrence
#'        to keep when duplicates are found.
#' @param quiet Logical; if TRUE, suppresses messages about duplicate IDs
#'
#' @return A data frame with duplicates removed according to the 'keep' parameter
#' @details The keep argumet will match full or partially,
#'  ie "la" = "last", "al" = "all", "fi", "fir" = "first"
#' @export
#'
#' @examples
#' testDF <- data.frame(
#' id_num = c(1, 1, 2, 2, 3, 4, 5, 5, 6),
#' value = c(100, 100, 200, 250, 300, 400, 500, 500, 600),
#' group = c("A", "A", "B", "B", "C", "D", "E", "F", "G")
#' )
#' remove_duplicates_if_exists(testDF, "id_num", "first")
#' remove_duplicates_if_exists(testDF, "id_num", "fir")
remove_duplicates_if_exists <- function(dataf, id_col,
                                        keep = c("first", "last", "none"),
                                        quiet = FALSE) {
  # Validate inputs
  if (!is.data.frame(dataf)) {
    stop("The 'dataf' argument must be a data frame.")
  }

  #partial or full match
  keep <- match.arg(keep)

  # Handle column selection with NSE (non-standard evaluation)
  id_expr <- substitute(id_col)

  # Convert string column name to symbol if passed as a string
  if (is.character(id_expr)) {
    if (!id_expr %in% names(dataf)) {
      stop("Column '", id_expr, "' not found in the data frame.")
    }
    id_col_name <- id_expr
    id_values <- dataf[[id_expr]]
  } else {
    # Handle column reference using dplyr-style NSE
    tryCatch({
      id_values <- eval(id_expr, dataf, parent.frame())
      id_col_name <- deparse(id_expr)
    }, error = function(e) {
      stop("Could not evaluate the ID expression: ", deparse(id_expr))
    })
  }

  # Find duplicate IDs
  dupe_mask <- duplicated(id_values) | duplicated(id_values, fromLast = TRUE)

  # If no duplicates, return original data
  if (!any(dupe_mask)) {
    if (!quiet) message("No duplicates found.")
    return(dataf)
  }

  # Get unique duplicate IDs
  duplicate_ids <- id_values[dupe_mask]
  unique_duplicate_ids <- unique(duplicate_ids)

  if (!quiet) {
    message("Found ", length(unique_duplicate_ids), " unique ID(s) with duplicates.")
  }

  # Analysis of duplicate rows (identical vs conflicting)
  if (!quiet) {
    message("Analyzing duplicates...")
    identical_rows_same_ids <- character(0)
    conflicting_rows_same_ids <- character(0)

    # Check each duplicate ID
    for (dup_id in unique_duplicate_ids) {
      # Find rows with this ID
      rows_with_id <- which(id_values == dup_id)

      # Extract just the data for comparison
      rows_data <- dataf[rows_with_id, , drop = FALSE]

      # Compare all rows with first row - much faster for large datasets
      first_row <- rows_data[1, , drop = FALSE]
      all_rows_identical <- TRUE

      for (i in 2:nrow(rows_data)) {
        if (!identical(as.list(first_row), as.list(rows_data[i, , drop = FALSE]))) {
          all_rows_identical <- FALSE
          break
        }
      }

      if (all_rows_identical) {
        identical_rows_same_ids <- c(identical_rows_same_ids, as.character(dup_id))
      } else {
        conflicting_rows_same_ids <- c(conflicting_rows_same_ids, as.character(dup_id))
      }
    }

    # Report findings
    if (length(identical_rows_same_ids) > 0) {
      message("Found IDs with duplicate identical rows: ",
              paste(if(length(identical_rows_same_ids) > 10)
                c(identical_rows_same_ids[1:10], "...") else identical_rows_same_ids,
                collapse = ", "))
    }

    if (length(conflicting_rows_same_ids) > 0) {
      message("WARNING: Found IDs with conflicting values in different rows: ",
              paste(if(length(conflicting_rows_same_ids) > 10)
                c(conflicting_rows_same_ids[1:10], "...") else conflicting_rows_same_ids,
                collapse = ", "))
    }
  }

  # Remove duplicates based on the keep parameter
  if (keep == "first") {
    result <- dataf[!duplicated(id_values), ]
    if (!quiet) message("Kept first occurrence of each duplicate ID.")
  } else if (keep == "last") {
    result <- dataf[!duplicated(id_values, fromLast = TRUE), ]
    if (!quiet) message("Kept last occurrence of each duplicate ID.")
  } else if (keep == "none") {
    result <- dataf[!dupe_mask, ]
    if (!quiet) message("Removed all rows with duplicate IDs.")
  }

  if (!quiet) {
    message("Removed ", nrow(dataf) - nrow(result), " duplicate rows.")
  }

  return(result)
}
