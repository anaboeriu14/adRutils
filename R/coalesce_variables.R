#' Coalesce related variables by pattern or manual grouping
#'
#' @param dataf A data frame containing the variables to coalesce
#' @param pattern_extract Regular expression identifying the varying part to remove
#' @param var_groups Named list of column groups to coalesce
#' @param prefix Character string to prepend to new column names (default: "")
#' @param force Logical, whether to reprocess if already completed (default: FALSE)
#' @param check_processed Logical, whether to check/register processing status (default: TRUE)
#' @param quiet Logical, whether to suppress informational messages (default: FALSE)
#'
#' @return Data frame with additional coalesced columns
#' @export
coalesce_variables <- function(dataf, pattern_extract = NULL, var_groups = NULL,
                               prefix = "", force = FALSE, check_processed = TRUE,
                               quiet = FALSE) {

  # Validate inputs
  .validate_coalesce_params(dataf, pattern_extract, var_groups,
                            prefix, force, check_processed, quiet)

  # Create groups
  groups <- .create_variable_groups(dataf, pattern_extract, var_groups)

  # Check if should skip
  if (.should_skip_processing(groups, force, check_processed, quiet)) {
    return(dataf)
  }

  # Coalesce and register
  result_df <- .coalesce_groups(dataf, groups, prefix, quiet)
  .register_if_enabled(groups, check_processed)

  return(result_df)
}

#' Validate coalesce_variables parameters
#' @keywords internal
.validate_coalesce_params <- function(dataf, pattern_extract, var_groups,
                                      prefix, force, check_processed, quiet) {
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
          is.logical(force) && length(force) == 1 &&
          is.logical(check_processed) && length(check_processed) == 1 &&
          is.logical(quiet) && length(quiet) == 1,
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

#' Check if processing should be skipped
#' @keywords internal
.should_skip_processing <- function(groups, force, check_processed, quiet) {
  if (length(groups) == 0) {
    if (!quiet) cli::cli_alert_warning("No matching columns found for coalescing")
    return(TRUE)
  }

  if (check_processed && !force) {
    if (is_processed("coalesce_variables", unlist(groups), error_if_exists = FALSE)) {
      if (!quiet) {
        cli::cli_alert_info("Variables already coalesced. Use {.code force = TRUE} to override")
      }
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Coalesce groups into new columns
#' @keywords internal
.coalesce_groups <- function(dataf, groups, prefix, quiet) {
  result_df <- dataf
  new_cols <- character()

  for (base_name in names(groups)) {
    cols <- groups[[base_name]]

    if (length(cols) > 1) {
      new_name <- paste0(prefix, base_name)

      if (new_name %in% names(result_df) && !quiet) {
        cli::cli_alert_warning("Overwriting existing column: {.field {new_name}}")
      }

      result_df[[new_name]] <- do.call(dplyr::coalesce, as.list(result_df[cols]))
      new_cols <- c(new_cols, new_name)
    }
  }

  if (length(new_cols) > 0 && !quiet) {
    prefix_msg <- if (prefix != "") paste0(' with prefix "', prefix, '"') else ""
    cli::cli_alert_success(
      "Created {length(new_cols)} coalesced column{?s}{prefix_msg}"
    )
  }

  return(result_df)
}

#' Register processing if enabled
#' @keywords internal
.register_if_enabled <- function(groups, check_processed) {
  if (check_processed) {
    register_processed("coalesce_variables", unlist(groups))
  }
  invisible(NULL)
}
