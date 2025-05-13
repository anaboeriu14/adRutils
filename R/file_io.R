#' Read and combine CSV files matching specified patterns
#'
#' @param directory_path Character. Path to the directory containing CSV files
#' @param missing_vals Character vector. Values to be treated as NA (required)
#' @param patterns List or character vector. Patterns to match files against.
#'    Each pattern will match any file containing that string.
#'    - Single pattern: "dataf_version_" will match all files containing this exact phrase
#'   - Multiple patterns: c("genetics", "clean_") will match ANY file containing EITHER "genetics" OR "clean_"
#'   - Regular expressions can be used: "Genetic_v[0-9]+" will match files "Genetic_v1", "Genetic_v2", etc
#' @param all_files Logical. If TRUE, reads all CSV files in directory regardless of patterns
#' @param clean_col_names Logical. If TRUE, cleans column names using janitor::clean_names()
#' @param guess_max Integer. Maximum number of rows to use for guessing column types
#' @param combine Logical. If TRUE, combines all dataframes into one. If FALSE, returns a named list
#' @param verbose Logical. If TRUE, prints progress messages
#'
#' @return A combined dataframe or a named list of dataframes
#' @export
read_csvs_by_pattern <- function(directory_path,
                             missing_vals,
                             patterns = NULL,
                             all_files = FALSE,
                             clean_col_names = TRUE,
                             guess_max = 5000,
                             combine = TRUE,
                             verbose = TRUE) {

  # Input validation
  if (!dir.exists(directory_path)) {
    stop("Directory does not exist: ", directory_path)
  }

  if (missing(missing_vals)) {
    stop("'missing_vals' parameter is required")
  }

  if (!is.character(missing_vals) && !is.factor(missing_vals)) {
    stop("'missing_vals' must be a character vector")
  }

  # File path collection
  if (all_files) {
    if (verbose) message("Reading all CSV files in directory")
    file_paths <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)
  } else if (!is.null(patterns)) {
    if (verbose) message("Creating pattern-based file list")
    file_paths <- c()

    # Convert to list if needed
    if (!is.list(patterns)) {
      patterns <- as.list(patterns)
    }

    # Process each pattern
    for (pattern in patterns) {
      if (verbose) message(paste("Processing pattern:", pattern))
      regex_pattern <- paste0(".*", pattern, ".*\\.csv$")
      matched_files <- list.files(directory_path, pattern = regex_pattern, full.names = TRUE)
      file_paths <- c(file_paths, matched_files)
    }

    # Remove duplicates
    file_paths <- unique(file_paths)
  } else {
    stop("Either provide patterns or set all_files = TRUE")
  }

  # Check if files were found
  if (length(file_paths) == 0) {
    warning("No files found matching the specified patterns")
    return(NULL)
  }

  if (verbose) message(paste("Found", length(file_paths), "files matching patterns"))

  # Create containers
  df_list <- list()
  names_list <- c()

  # Process each file
  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    file_name <- basename(file_path)
    names_list <- c(names_list, file_name)

    if (verbose) message(paste("Reading file:", file_name))

    # Read CSV
    df <- readr::read_csv(file_path, guess_max = guess_max, na = missing_vals)

    # Clean column names if requested
    if (clean_col_names) {
      df <- janitor::clean_names(df)
    }

    # Store dataframe
    df_list[[i]] <- df
  }

  # Return results
  if (length(df_list) > 0) {
    if (combine) {
      if (verbose) message("Combining all dataframes")
      combined_df <- do.call(rbind, df_list)
      return(combined_df)
    } else {
      if (verbose) message("Returning list of dataframes")
      names(df_list) <- names_list
      return(df_list)
    }
  } else {
    warning("No files could be read successfully")
    return(NULL)
  }
}
