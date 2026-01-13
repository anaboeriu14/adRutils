#' Create ID Mapping Table
#'
#' Extract unique ID pairs from a dataset with multiple ID formats.
#'
#' @param data Data frame containing both ID columns
#' @param id_col1 Name of first ID column
#' @param id_col2 Name of second ID column
#' @param trim Logical. Trim whitespace? (default: TRUE)
#'
#' @return Data frame with two columns of unique ID pairs (short & long id)
#' @export
create_id_mapping <- function(data, id_col1, id_col2, trim = TRUE) {

  validate_params(
    data = data,
    columns = c(id_col1, id_col2),
    custom_checks = list(
      list(
        condition = id_col1 != id_col2,
        message = "ID columns must be different"
      )
    ),
    context = "create_id_mapping"
  )

  # Extract and prepare ID pairs
  mapping <- .extract_id_pairs(data, id_col1, id_col2, trim)

  # Check both directions for duplicates
  .check_mapping_duplicates(mapping, id_col1)
  .check_mapping_duplicates(mapping, id_col2)

  # Report success
  cli::cli_alert_success(
    "Created mapping with {nrow(mapping)} unique ID pair{?s}"
  )

  return(mapping)
}

#' Add ID Column Using ID Mapping
#'
#' Add a new ID column to data by looking up values from an ID mapping table.
#' Useful when merging datasets that use different ID conventions.
#'
#' @param data Data frame to add ID to
#' @param id_mapping Data frame with ID lookup table (must contain existing_id_col and id_col_to_add)
#' @param existing_id_col Name of existing ID column in data
#' @param id_col_to_add Name of ID column to add from mapping
#' @param trim Logical. Trim whitespace before matching? (default: TRUE)
#' @param quiet Logical. Suppress matching statistics? (default: FALSE)
#'
#' @return Data frame with new ID column added
#' @export
add_id_mapping <- function(data,
                           id_mapping,
                           existing_id_col,
                           id_col_to_add,
                           trim = TRUE,
                           quiet = FALSE) {

  # Validate inputs
  validate_params(
    data = data,
    columns = existing_id_col,
    custom_checks = list(
      list(
        condition = is.data.frame(id_mapping),
        message = "{.arg id_mapping} must be a data frame"
      ),
      list(
        condition = existing_id_col %in% names(id_mapping) && id_col_to_add %in% names(id_mapping),
        message = "Both {.arg existing_id_col} and {.arg id_col_to_add} must exist in id_mapping"
      ),
      list(
        condition = existing_id_col != id_col_to_add,
        message = "{.arg existing_id_col} and {.arg id_col_to_add} must be different"
      )
    ),
    context = "add_id_mapping"
  )

  # Warn if overwriting
  if (id_col_to_add %in% names(data) && !quiet) {
    cli::cli_alert_warning("Overwriting existing column: {.field {id_col_to_add}}")
  }

  # Prepare data and mapping
  mapping_prep <- .prepare_mapping(id_mapping, existing_id_col, id_col_to_add, trim)
  data_prep <- .prepare_data_for_join(data, existing_id_col, trim)

  # Check for issues
  if (!quiet) .check_mapping_duplicates(mapping_prep, existing_id_col)

  # Perform join
  result <- .join_with_mapping(data_prep, mapping_prep, existing_id_col, id_col_to_add)

  # Report results
  if (!quiet) .report_id_matching(result, id_col_to_add)

  return(result)
}

#' Extract and prepare ID pairs from data
#' @keywords internal
#' @noRd
.extract_id_pairs <- function(data, id_col1, id_col2, trim) {
  mapping <- data %>%
    select(all_of(c(id_col1, id_col2))) %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  if (trim) {
    mapping <- mapping %>%
      mutate(across(everything(), str_trim))
  }

  return(mapping)
}

#' Prepare mapping table for join
#' @keywords internal
#' @noRd
.prepare_mapping <- function(id_mapping, existing_id_col, id_col_to_add, trim) {
  mapping <- id_mapping %>%
    select(all_of(c(existing_id_col, id_col_to_add))) %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  if (trim) {
    mapping <- mapping %>%
      mutate(across(everything(), str_trim))
  }

  return(mapping)
}

#' Prepare data for ID join
#' @keywords internal
#' @noRd
.prepare_data_for_join <- function(data, existing_id_col, trim) {
  if (trim) {
    data %>%
      mutate(!!existing_id_col := str_trim(as.character(.data[[existing_id_col]])))
  } else {
    data %>%
      mutate(!!existing_id_col := as.character(.data[[existing_id_col]]))
  }
}

#' Join data with mapping and handle column conflicts
#' @keywords internal
#' @noRd
.join_with_mapping <- function(data, mapping, existing_id_col, id_col_to_add) {
  result <- data %>%
    left_join(mapping, by = existing_id_col, suffix = c("", ".new"))

  # Handle overwrite suffix if it occurred
  new_col_name <- paste0(id_col_to_add, ".new")
  if (new_col_name %in% names(result)) {
    result <- result %>%
      select(-all_of(id_col_to_add)) %>%
      rename(!!id_col_to_add := !!new_col_name)
  }

  return(result)
}

#' Check for duplicate IDs in mapping
#' @keywords internal
#' @noRd
.check_mapping_duplicates <- function(mapping, id_col) {
  n_dupes <- sum(duplicated(mapping[[id_col]]))

  if (n_dupes > 0) {
    cli::cli_alert_warning(
      "{n_dupes} duplicate{?s} in {.field {id_col}} - one-to-many mapping"
    )
  }

  invisible(NULL)
}

#' Report ID matching statistics
#' @keywords internal
#' @noRd
.report_id_matching <- function(data, id_col) {
  n_matched <- sum(!is.na(data[[id_col]]))
  n_total <- nrow(data)

  if (n_matched < n_total) {
    n_unmatched <- n_total - n_matched
    pct_unmatched <- round((n_unmatched / n_total) * 100, 1)
    cli::cli_alert_warning(
      "{n_unmatched} row{?s} ({pct_unmatched}%) unmatched"
    )
  } else {
    cli::cli_alert_success("All {n_total} row{?s} matched")
  }

  invisible(NULL)
}
