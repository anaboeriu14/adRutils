#' Read and combine CSV files matching specified patterns
#'
#' @param directory_path Character. Path to the directory containing CSV files
#' @param missing_vals Character vector. Values to be treated as NA (required)
#' @param patterns Character vector. Patterns to match files against (regex supported).
#'   Use NULL with all_files = TRUE to read all CSVs
#' @param all_files Logical. If TRUE, reads all CSV files regardless of patterns
#' @param clean_col_names Logical. If TRUE, cleans column names using janitor::clean_names()
#' @param guess_max Integer. Maximum number of rows to use for guessing column types
#' @param combine Logical. If TRUE, combines all dataframes into one. If FALSE, returns a named list
#' @param verbose Logical. If TRUE, prints progress messages (default: FALSE)
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
                                 verbose = FALSE) {

  .validate_csv_inputs(directory_path, missing_vals, patterns, all_files)

  file_paths <- .find_csv_files(directory_path, patterns, all_files)

  if (length(file_paths) == 0) {
    cli::cli_alert_warning("No files found matching the specified patterns")
    return(NULL)
  }

  if (verbose) {
    cli::cli_alert_info("Reading {length(file_paths)} file{?s}")
  }

  df_list <- .load_csv_files(file_paths, missing_vals, clean_col_names,
                             guess_max, verbose)

  .format_csv_output(df_list, file_paths, combine, verbose)
}

#' Validate input parameters for read_csvs_by_pattern
#' @keywords internal
.validate_csv_inputs <- function(directory_path, missing_vals, patterns, all_files) {
  validate_params(
    custom_checks = list(
      list(
        condition = dir.exists(directory_path),
        message = "Directory does not exist: {directory_path}"
      ),
      list(
        condition = !missing(missing_vals),
        message = "{.arg missing_vals} parameter is required"
      ),
      list(
        condition = is.character(missing_vals) || is.factor(missing_vals),
        message = "{.arg missing_vals} must be a character vector"
      ),
      list(
        condition = !is.null(patterns) || all_files,
        message = "Either provide {.arg patterns} or set {.arg all_files} = TRUE"
      )
    ),
    context = "read_csvs_by_pattern"
  )
}

#' Find CSV files matching patterns
#' @keywords internal
.find_csv_files <- function(directory_path, patterns, all_files) {
  if (all_files) {
    return(list.files(directory_path, pattern = "\\.csv$", full.names = TRUE))
  }

  regex_pattern <- paste0(".*(?:", paste(patterns, collapse = "|"), ").*\\.csv$")
  matched_files <- list.files(directory_path, pattern = regex_pattern,
                              full.names = TRUE)
  return(unique(matched_files))
}

#' Load CSV files into data frames
#' @keywords internal
.load_csv_files <- function(file_paths, missing_vals, clean_col_names,
                            guess_max, verbose) {

  n_files <- length(file_paths)

  purrr::map(seq_along(file_paths), function(i) {
    file_path <- file_paths[i]
    file_name <- basename(file_path)

    if (verbose) {
      # Show progress for many files, individual messages for few
      if (n_files > 10) {
        if (i == 1) cli::cli_progress_bar("Reading files", total = n_files)
        cli::cli_progress_update()
      } else {
        cli::cli_alert("Reading file {i}/{n_files}: {.file {file_name}}")
      }
    }

    df <- readr::read_csv(file_path, guess_max = guess_max, na = missing_vals,
                          show_col_types = FALSE)

    if (clean_col_names) df <- janitor::clean_names(df)

    return(df)
  })
}

#' Format CSV results for output
#' @keywords internal
.format_csv_output <- function(df_list, file_paths, combine, verbose) {
  if (length(df_list) == 0) {
    cli::cli_abort("No files could be read successfully")
  }

  if (combine) {
    if (verbose) cli::cli_alert_success("Combining all dataframes")
    return(dplyr::bind_rows(df_list))
  }

  if (verbose) cli::cli_alert_success("Returning list of dataframes")
  names(df_list) <- basename(file_paths)

  return(df_list)
}
