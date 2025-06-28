#' Coalesce related variables by pattern or manual grouping
#'
#' @param dataf A data frame containing the variables to coalesce
#' @param pattern_extract Regular expression identifying the varying part to remove
#' @param var_groups Named list of column groups to coalesce
#' @param prefix Character string to prepend to new column names (default: "")
#' @param force Logical, whether to reprocess if already completed (default: FALSE)
#' @param check_processed Logical, whether to check/register processing status (default: TRUE)
#'
#' @return Data frame with additional coalesced columns
#' @export
coalesce_variables <- function(dataf, pattern_extract = NULL, var_groups = NULL,
                               prefix = "", force = FALSE, check_processed = TRUE) {

  # Input validation
  .validate_coalesce_params(dataf, pattern_extract, var_groups, prefix, force, check_processed)

  # Create groups
  groups <- .create_variable_groups(dataf, pattern_extract, var_groups)

  # Early returns
  if (.should_skip_processing(groups, dataf, force, check_processed)) return(dataf)

  # Process groups and return result
  result_df <- .coalesce_groups(dataf, groups, prefix)

  # Register processing
  .register_if_enabled(groups, check_processed)

  return(result_df)
}

#' Validate coalesce_variables parameters
#' @keywords internal
.validate_coalesce_params <- function(dataf, pattern_extract, var_groups,
                                      prefix, force, check_processed) {
  adRutils::validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = !is.null(pattern_extract) || !is.null(var_groups),
        message = "Either pattern_extract or var_groups must be provided"
      ),
      list(
        condition = is.null(pattern_extract) || is.null(var_groups),
        message = "Provide either pattern_extract OR var_groups, not both"
      ),
      list(
        condition = is.null(pattern_extract) || (is.character(pattern_extract) && length(pattern_extract) == 1 && nchar(pattern_extract) > 0),
        message = "pattern_extract must be a single non-empty character string"
      ),
      list(
        condition = is.null(var_groups) || (is.list(var_groups) && length(names(var_groups)) > 0),
        message = "var_groups must be a named list with at least one element"
      ),
      list(
        condition = is.character(prefix) && length(prefix) == 1 &&
          is.logical(force) && length(force) == 1 &&
          is.logical(check_processed) && length(check_processed) == 1,
        message = "prefix must be character, force and check_processed must be single logical values"
      )
    ),
    context = "coalesce_variables"
  )
}

#' Create groups from pattern matching
#' @keywords internal
.create_groups_from_pattern <- function(dataf, pattern_extract) {
  matching_cols <- grep(pattern_extract, names(dataf), value = TRUE)

  if (length(matching_cols) == 0) {
    warning("No columns found matching pattern: ", pattern_extract)
    return(list())
  }

  groups <- list()
  for (col in matching_cols) {
    parts <- stringr::str_split(col, pattern_extract)[[1]]
    if (length(parts) == 2) {
      base_name <- paste0(parts[1], parts[2])
      groups[[base_name]] <- c(groups[[base_name]], col)
    }
  }

  return(groups)
}

#' Create variable groups from pattern or manual specification
#' @keywords internal
.create_variable_groups <- function(dataf, pattern_extract, var_groups) {
  if (!is.null(pattern_extract)) {
    return(.create_groups_from_pattern(dataf, pattern_extract))
  } else {
    return(.validate_and_return_manual_groups(dataf, var_groups))
  }
}

#' Validate and return manual groups
#' @keywords internal
.validate_and_return_manual_groups <- function(dataf, var_groups) {
  adRutils::validate_params(
    data = dataf,
    columns = unlist(var_groups),
    context = "coalesce_variables"
  )
  return(var_groups)
}

#' Check if processing should be skipped
#' @keywords internal
.should_skip_processing <- function(groups, dataf, force, check_processed) {
  # No groups found
  if (length(groups) == 0) {
    warning("No groups found for coalescing")
    return(TRUE)
  }

  # Already processed check
  if (check_processed && !force) {
    all_cols <- unlist(groups)
    if (adRutils::is_processed("coalesce_variables", all_cols, error_if_exists = FALSE)) {
      message("Variables already coalesced. Use force=TRUE to override.")
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Coalesce groups into new columns
#' @keywords internal
.coalesce_groups <- function(dataf, groups, prefix) {
  result_df <- dataf

  for (base_name in names(groups)) {
    cols <- groups[[base_name]]
    if (length(cols) > 1) {
      new_name <- paste0(prefix, base_name)
      .warn_if_overwriting(new_name, result_df)
      result_df[[new_name]] <- do.call(dplyr::coalesce, result_df[cols])
      message("Coalesced ", length(cols), " columns into '", new_name, "'")
    }
  }
  return(result_df)
}

#' Warn if column will be overwritten
#' @keywords internal
.warn_if_overwriting <- function(new_name, dataf) {
  if (new_name %in% names(dataf)) {
    warning("Column '", new_name, "' will be overwritten.")
  }
}

#' Register processing if enabled
#' @keywords internal
.register_if_enabled <- function(groups, check_processed) {
  if (check_processed) {
    adRutils::register_processed("coalesce_variables", unlist(groups))
  }
}
