#' Coalesce related variables by pattern or manual grouping
#'
#' @param dataf A data frame containing the variables to coalesce
#' @param pattern_extract Regular expression identifying the varying part to remove
#' @param var_groups Named list of column groups to coalesce
#' @param prefix Character string to prepend to new column names (default: "")
#' @param overwrite Logical. If TRUE, overwrites existing coalesced columns (default: FALSE)
#' @param quiet Logical. If TRUE, suppresses messages (default: FALSE)
#'
#' @return Data frame with additional coalesced columns
#' @export
coalesce_variables <- function(dataf, pattern_extract = NULL, var_groups = NULL,
                               prefix = "", overwrite = FALSE, quiet = FALSE) {

  # Validate inputs
  .validate_coalesce_params(dataf, pattern_extract, var_groups,
                            prefix, overwrite, quiet)

  # Create groups
  groups <- .create_variable_groups(dataf, pattern_extract, var_groups)

  # Check if should skip
  if (length(groups) == 0) {
    if (!quiet) cli::cli_alert_warning("No matching columns found for coalescing")
    return(dataf)
  }

  # Coalesce
  result_df <- .coalesce_groups(dataf, groups, prefix, overwrite, quiet)

  return(result_df)
}

#' Validate coalesce_variables parameters
#' @keywords internal
.validate_coalesce_params <- function(dataf, pattern_extract, var_groups,
                                      prefix, overwrite, quiet) {  # ← Removed check_processed
  validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = !is.null(pattern_extract) || !is.null(var_groups),
        message = "Either {.arg pattern_extract} or {.arg var_groups} must be provided"
      ),
      list(
        condition = is.null(pattern_extract) || is.null(var_groups),
        message = "Provide either {.arg pattern_extract} OR {.arg var_groups}, not both"
      ),
      list(
        condition = is.null(pattern_extract) ||
          (is.character(pattern_extract) && length(pattern_extract) == 1 &&
             nchar(pattern_extract) > 0),
        message = "{.arg pattern_extract} must be a single non-empty character string"
      ),
      list(
        condition = is.null(var_groups) ||
          (is.list(var_groups) && length(names(var_groups)) > 0),
        message = "{.arg var_groups} must be a named list"
      ),
      list(
        condition = is.character(prefix) && length(prefix) == 1 &&
          is.logical(overwrite) && length(overwrite) == 1 &&
          is.logical(quiet) && length(quiet) == 1,  # ← Removed check_processed
        message = "All parameters must be scalar values of correct type"
      )
    ),
    context = "coalesce_variables"
  )

  invisible(TRUE)
}

#' Build variable groups from pattern or manual specification
#' @keywords internal
.create_variable_groups <- function(dataf, pattern_extract, var_groups) {
  if (!is.null(pattern_extract)) {
    return(.build_groups_from_pattern(dataf, pattern_extract))
  }

  # Validate manually specified groups
  validate_params(
    data = dataf,
    columns = unlist(var_groups),
    context = "coalesce_variables"
  )

  return(var_groups)
}

#' Build groups from pattern matching
#' @keywords internal
.build_groups_from_pattern <- function(dataf, pattern_extract) {
  matching_cols <- grep(pattern_extract, names(dataf), value = TRUE)

  if (length(matching_cols) == 0) {
    return(list())
  }

  groups <- list()
  for (col in matching_cols) {
    parts <- stringr::str_split(col, pattern_extract, n = 2)[[1]]
    if (length(parts) == 2) {
      base_name <- paste0(parts[1], parts[2])
      groups[[base_name]] <- c(groups[[base_name]], col)
    }
  }

  return(groups)
}

#' Coalesce groups into new columns
#' @keywords internal
.coalesce_groups <- function(dataf, groups, prefix, overwrite, quiet) {
  result_df <- dataf
  created_cols <- character()
  overwritten_cols <- character()
  skipped_cols <- character()

  for (base_name in names(groups)) {
    cols <- groups[[base_name]]

    if (length(cols) > 1) {
      new_name <- paste0(prefix, base_name)

      # Check if column already exists
      if (new_name %in% names(result_df)) {
        if (!overwrite) {
          skipped_cols <- c(skipped_cols, new_name)
          next  # Skip this one
        }
        overwritten_cols <- c(overwritten_cols, new_name)
      } else {
        created_cols <- c(created_cols, new_name)
      }

      result_df[[new_name]] <- do.call(dplyr::coalesce, as.list(result_df[cols]))
    }
  }

  # Report what happened
  if (length(skipped_cols) > 0 && !quiet) {
    cli::cli_alert_warning(
      "Skipped {length(skipped_cols)} column{?s} that already exist{?s}: {.field {skipped_cols}}"
    )
    cli::cli_alert_info("Use {.code overwrite = TRUE} to replace existing columns")
  }

  if (length(overwritten_cols) > 0 && !quiet) {
    cli::cli_alert_warning(
      "Overwriting existing column{?s}: {.field {overwritten_cols}}"
    )
  }

  total_created <- length(created_cols) + length(overwritten_cols)
  if (total_created > 0 && !quiet) {
    prefix_msg <- if (prefix != "") paste0(' with prefix "', prefix, '"') else ""
    cli::cli_alert_success(
      "Created {total_created} coalesced column{?s}{prefix_msg}"
    )
  }

  return(result_df)
}
