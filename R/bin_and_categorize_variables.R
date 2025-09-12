#' Bin and Categorize Variables into Groups
#'
#' Transforms variables into categorical groups using cutpoints for continuous variables
#'  and value mappings for categorical variables.
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
#'
#' @return A data frame with added grouping variables
#'
#' @export
bin_and_categorize_variables <- function(dataf, groups, filter_missing = FALSE) {
  # Check if input is a data frame
  if (!is.data.frame(dataf)) {
    cli::cli_abort("Input {.ard dataf} must be a data frame")
  }

  # Check if groups is a list
  if (!is.list(groups)) {
    cli::cli_abort("{.arg groups} must be a list of group specifications")
  }

  # Make a copy of the data
  result <- dataf

  # Track created group columns
  created_columns <- character(0)

  # Process each group
  for (group in groups) {
    # Check required fields
    if (is.null(group$col) || is.null(group$type)) {
      cli::cli_abort("Each group must have {.field col} and {.field type} specified")
    }

    if (!group$col %in% names(dataf)) {
      cli::cli_abort("Column {.val {group$col}} not found in data")
    }

    # Set output column name
    out_name <- ifelse(is.null(group$name), paste0(group$col, "_group"), group$name)

    # Create grouping based on type
    if (group$type == "cutpoints") {
      # Check for cutpoints
      if (is.null(group$cutpoints)) {
        cli::cli_abort("{.field cutpoints} required for type {.val cutpoints}")
      }

      # Add negative and positive infinity to create complete ranges
      breaks <- c(-Inf, group$cutpoints, Inf)

      # Create default labels if not provided
      if (is.null(group$labels)) {
        labels <- character(length(breaks) - 1)
        for (i in 1:(length(breaks) - 1)) {
          if (i == 1) {
            labels[i] <- paste0("<=", group$cutpoints[i])
          } else if (i == length(breaks) - 1) {
            labels[i] <- paste0(">=", (group$cutpoints[i-1] + 1))
          } else {
            labels[i] <- paste0("[", (group$cutpoints[i-1] + 1), ",", group$cutpoints[i], "]")
          }
        }
      } else {
        labels <- group$labels
        if (length(labels) != length(breaks) - 1) {
          cli::cli_abort("Number of labels must match number of groups (cutpoints + 1)")
        }
      }

      # Create groups
      result[[out_name]] <- cut(result[[group$col]],
                                breaks = breaks,
                                labels = labels,
                                include.lowest = TRUE)

    } else if (group$type == "categorical") {
      # Check for values mapping
      if (is.null(group$values)) {
        cli::cli_abort("{.field values} required for type {.val categorical}")
      }

      # Create a character vector first
      result[[out_name]] <- as.character(result[[group$col]])

      # Apply mappings
      for (val in names(group$values)) {
        result[[out_name]][result[[out_name]] == val] <- group$values[val]
      }

      # Convert to factor
      result[[out_name]] <- factor(result[[out_name]])

    } else if (group$type == "custom") {
      # Check for custom function
      if (is.null(group$custom_fn) || !is.function(group$custom_fn)) {
        cli::cli_abort("{.field custom_fn} required for type {.val custom}")
      }

      # Apply custom function
      result[[out_name]] <- group$custom_fn(result[[group$col]])

      # Convert to factor if result is character
      if (is.character(result[[out_name]])) {
        result[[out_name]] <- factor(result[[out_name]])
      }
    } else {
      cli::cli_abort("Unknown group type: {.val {group$type}}")
    }

    # Add to list of created columns
    created_columns <- c(created_columns, out_name)
  }

  # Filter missing values if requested
  if (filter_missing && length(created_columns) > 0) {
    result <- result[complete.cases(result[created_columns]), ]
  }

  return(result)
}
