#' Bin and Categorize Variables into Groups
#'
#' Transforms variables into categorical groups using cutpoints for continuous variables
#' and value mappings for categorical variables.
#'
#' @param dataf A data frame containing demographic variables
#' @param groups List of group specifications. Each element should be a list with:
#'        - 'col': String. Column name in the data
#'        - 'name': String. Name for the new grouping variable (optional, defaults to col_group)
#'        - 'type': String. One of "cutpoints" (for continuous), "categorical", or "custom"
#'        - 'cutpoints': For continuous variables, numeric vector of cutpoints (required for "cutpoints" type)
#'        - 'labels': Labels for the groups (optional)
#'        - 'values': For categorical variables, named vector mapping codes to labels (required for "categorical" type)
#'        - 'custom_fn': For custom grouping, a function that takes a vector and returns grouped values
#' @param filter_missing Logical. Whether to filter rows with NA in any group column (default: FALSE)
#' @param quiet Logical. Whether to suppress informational messages (default: FALSE)
#'
#' @return A data frame with added grouping variables
#'
#' @export
bin_and_categorize_variables <- function(dataf, groups, filter_missing = FALSE,
                                         quiet = FALSE) {
  validate_params(
    data = dataf,
    custom_checks = list(
      list(
        condition = is.list(groups) && length(groups) > 0,
        message = "{.arg groups} must be a non-empty list of group specifications"
      )
    ),
    context = "bin_and_categorize_variables"
  )

  result <- dataf
  created_columns <- character()
  overwritten_columns <- character()

  for (group in groups) {
    .validate_group_spec(group, dataf)

    out_name <- group$name %||% paste0(group$col, "_group")

    # Track overwrites
    if (out_name %in% names(result)) {
      overwritten_columns <- c(overwritten_columns, out_name)
    }

    result[[out_name]] <- .create_grouped_variable(result[[group$col]], group)
    created_columns <- c(created_columns, out_name)
  }

  # Warn about overwrites
  if (!quiet && length(overwritten_columns) > 0) {
    cli::cli_alert_warning(
      "Overwriting existing column{?s}: {.field {overwritten_columns}}"
    )
  }

  # Filter missing values if requested
  if (filter_missing && length(created_columns) > 0) {
    n_before <- nrow(result)
    result <- result[complete.cases(result[created_columns]), ]
    n_removed <- n_before - nrow(result)

    if (!quiet && n_removed > 0) {
      cli::cli_alert_info("Filtered {n_removed} row{?s} with missing values")
    }
  }

  # Report success
  if (!quiet && length(created_columns) > 0) {
    cli::cli_alert_success(
      "Created {length(created_columns)} grouped variable{?s}"
    )
  }

  return(result)
}

#' Validate individual group specification
#' @keywords internal
#' @noRd
.validate_group_spec <- function(group, dataf) {
  # Check required fields
  if (is.null(group$col) || is.null(group$type)) {
    cli::cli_abort("Each group must have {.field col} and {.field type} specified")
  }

  if (!group$col %in% names(dataf)) {
    cli::cli_abort("Column {.field {group$col}} not found in data")
  }

  # Validate type-specific requirements
  if (group$type == "cutpoints" && is.null(group$cutpoints)) {
    cli::cli_abort("{.field cutpoints} required for type {.val cutpoints}")
  }

  if (group$type == "categorical" && is.null(group$values)) {
    cli::cli_abort("{.field values} required for type {.val categorical}")
  }

  if (group$type == "custom" && (is.null(group$custom_fn) || !is.function(group$custom_fn))) {
    cli::cli_abort("{.field custom_fn} (function) required for type {.val custom}")
  }

  if (!group$type %in% c("cutpoints", "categorical", "custom")) {
    cli::cli_abort("Unknown group type: {.val {group$type}}")
  }

  invisible(TRUE)
}

#' Create grouped variable based on type
#' @keywords internal
#' @noRd
.create_grouped_variable <- function(column_data, group) {
  switch(group$type,
    cutpoints = .bin_by_cutpoints(column_data, group$cutpoints, group$labels),
    categorical = .recode_categorical(column_data, group$values),
    custom = .apply_custom_function(column_data, group$custom_fn)
  )
}

#' Bin continuous variable using cutpoints
#' @keywords internal
#' @noRd
.bin_by_cutpoints <- function(column_data, cutpoints, labels = NULL) {
  breaks <- c(-Inf, cutpoints, Inf)

  # Generate default labels if not provided
  if (is.null(labels)) {
    labels <- .generate_cutpoint_labels(cutpoints, breaks)
  } else {
    .validate_label_count(labels, breaks)
  }

  cut(column_data, breaks = breaks, labels = labels, include.lowest = TRUE)
}

#' Generate default labels for cutpoints
#' @keywords internal
#' @noRd
.generate_cutpoint_labels <- function(cutpoints, breaks) {
  n_groups <- length(breaks) - 1
  labels <- character(n_groups)

  for (i in seq_len(n_groups)) {
    if (i == 1) {
      labels[i] <- paste0("<=", cutpoints[i])
    } else if (i == n_groups) {
      labels[i] <- paste0(">=", cutpoints[i - 1] + 1)
    } else {
      labels[i] <- paste0("[", cutpoints[i - 1] + 1, ",", cutpoints[i], "]")
    }
  }

  return(labels)
}

#' Validate label count matches number of groups
#' @keywords internal
#' @noRd
.validate_label_count <- function(labels, breaks) {
  expected_count <- length(breaks) - 1
  if (length(labels) != expected_count) {
    cli::cli_abort(
      "Number of labels ({length(labels)}) must match number of groups ({expected_count})"
    )
  }
  invisible(TRUE)
}

#' Recode categorical variable using value mapping
#' @keywords internal
#' @noRd
.recode_categorical <- function(column_data, value_map) {
  # Convert to character for mapping
  recoded <- as.character(column_data)

  # Apply each mapping
  for (old_val in names(value_map)) {
    recoded[recoded == old_val] <- value_map[[old_val]]
  }

  # Convert to factor
  factor(recoded)
}

#' Apply custom grouping function
#' @keywords internal
#' @noRd
.apply_custom_function <- function(column_data, custom_fn) {
  result <- custom_fn(column_data)

  # Convert to factor if character
  if (is.character(result)) {
    result <- factor(result)
  }

  return(result)
}
