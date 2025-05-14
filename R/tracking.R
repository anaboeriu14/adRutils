#' Get environment for tracking processed variables
#'
#' @return An environment object for storing processing status
#' @keywords internal
get_processed_vars_env <- function() {
  if (!exists(".adR_processed_vars", envir = .GlobalEnv)) {
    .GlobalEnv$.adR_processed_vars <- new.env(parent = emptyenv())
  }
  .GlobalEnv$.adR_processed_vars
}

#' Check if variables have been processed
#'
#' Checks whether the specified variables have already been processed
#' by a given function.
#'
#' @param func_name String. Name of the processing function
#' @param vars Character vector. Names of variables being processed
#' @param error_if_exists Logical. If TRUE, throws an error if already processed
#'
#' @return Logical. TRUE if variables have been processed, FALSE otherwise
#' @export
is_processed <- function(func_name, vars, error_if_exists = FALSE) {
  # Sort variable names for consistency
  sorted_vars <- sort(vars)

  # Create a unique key for this function + variables combination
  vars_key <- sprintf("%s_(%s)", func_name, paste(sorted_vars, collapse = "__"))

  # Get tracking environment
  envir_vars <- get_processed_vars_env()

  # Check direct match
  direct_match <- exists(vars_key, envir = envir_vars)

  # Check if any subsets have been processed
  all_keys <- ls(envir = envir_vars)
  func_keys <- all_keys[grepl(paste0("^", func_name, "_\\("), all_keys)]

  subset_match <- FALSE
  if (length(func_keys) > 0) {
    subset_match <- any(sapply(func_keys, function(existing_key) {
      # Extract variables from key
      existing_vars <- strsplit(
        sub(paste0(func_name, "_\\("), "", sub("\\)$", "", existing_key)),  "__" )[[1]]

      # Check if all current vars are in the existing set
      all(sorted_vars %in% existing_vars)
    }))
  }

  # Handle result
  is_proc <- direct_match || subset_match

  if (is_proc && error_if_exists) {
    if (direct_match) {
      stop("These variables have already been processed by ", func_name, ": ",
           paste(vars, collapse = ", "))
    } else {
      stop("Some or all of these variables have already been processed by ", func_name)
    }
  }

  return(is_proc)
}

#' Register variables as processed
#'
#' Marks a set of variables as having been processed by a specific function.
#'
#' @param func_name String. Name of the processing function
#' @param vars Character vector. Names of variables being processed
#'
#' @return Invisible NULL
#' @export
register_processed <- function(func_name, vars) {
  # Sort variable names for consistency
  sorted_vars <- sort(vars)

  # Create a unique key for this function + variables combination
  vars_key <- sprintf("%s_(%s)", func_name, paste(sorted_vars, collapse = "__"))

  # Get the environment
  envir_vars <- get_processed_vars_env()

  # Register as processed
  assign(vars_key, TRUE, envir = envir_vars)

  invisible(NULL)
}

#' Reset processing history
#'
#' Resets the processing history for a function or clears all history.
#'
#' @param func_name String, optional. Function name to clear history for
#' @param vars Character vector, optional. Variable names to clear history for
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' # Reset processing history for transform_log10
#' reset_processing("transform_log10")
#'
#' # Reset specific variables for a function
#' reset_processing("transform_log10", c("protein_a", "protein_b"))
#'
#' # Reset all processing history
#' reset_processing()
reset_processing <- function(func_name = NULL, vars = NULL) {
  envir_vars <- get_processed_vars_env()

  if (is.null(func_name) && is.null(vars)) {
    # Clear all processing history
    rm(list = ls(envir = envir_vars), envir = envir_vars)
  } else if (!is.null(func_name) && is.null(vars)) {
    # Clear history for specific function
    keys_to_remove <- ls(envir = envir_vars)[grepl(paste0("^", func_name, "_\\("), ls(envir = envir_vars))]
    if (length(keys_to_remove) > 0) {
      rm(list = keys_to_remove, envir = envir_vars)
    }
  } else if (!is.null(func_name) && !is.null(vars)) {
    # Clear history for specific function + variables
    sorted_vars <- sort(vars)
    vars_key <- sprintf("%s_(%s)", func_name, paste(sorted_vars, collapse = "__"))
    if (exists(vars_key, envir = envir_vars)) {
      rm(list = vars_key, envir = envir_vars)
    }
  }

  invisible(NULL)
}
