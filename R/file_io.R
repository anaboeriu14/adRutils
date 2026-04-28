#' Read and combine CSV files matching a pattern
#'
#' Reads all CSV files in a directory whose names match any of the supplied
#' regex patterns (or all CSVs if `all_files = TRUE`), optionally cleans
#' column names with [janitor::clean_names()], and either combines them
#' into a single data frame or returns them as a named list.
#'
#' @param directory_path Path to the directory containing CSV files.
#' @param missing_vals Character vector of strings to treat as `NA`.
#' @param patterns Character vector of regex patterns matching filenames.
#'   Required unless `all_files = TRUE`.
#' @param all_files If `TRUE`, read every `.csv` in the directory regardless
#'   of `patterns`. Default `FALSE`.
#' @param clean_col_names If `TRUE`, clean column names with
#'   [janitor::clean_names()]. Default `TRUE`.
#' @param combine If `TRUE` (default), bind all files into a single data
#'   frame using [dplyr::bind_rows()]. If `FALSE`, return a named list.
#' @param quiet If `TRUE`, suppress progress messages. Default `FALSE`.
#'
#' @return A combined data frame (when `combine = TRUE`), a named list of
#'   data frames (when `combine = FALSE`), or an empty data frame if no
#'   files matched.
#'
#' @export
read_csvs_by_pattern <- function(directory_path,
                                 missing_vals,
                                 patterns        = NULL,
                                 all_files       = FALSE,
                                 clean_col_names = TRUE,
                                 combine         = TRUE,
                                 quiet           = FALSE) {

  validate_args(
    directory_path  = is_string(),
    missing_vals    = is_nonempty_character(),
    all_files       = is_flag(),
    clean_col_names = is_flag(),
    combine         = is_flag(),
    quiet           = is_flag(),
    custom_checks = list(
      list(
        condition = dir.exists(directory_path),
        message   = "Directory does not exist: {.path {directory_path}}"
      ),
      list(
        condition = is.null(patterns) ||
          (is.character(patterns) && length(patterns) > 0L),
        message   = "{.arg patterns} must be NULL or a non-empty character vector"
      ),
      list(
        condition = !is.null(patterns) || all_files,
        message   = "Provide {.arg patterns} or set {.arg all_files = TRUE}"
      )
    )
  )

  file_paths <- .find_csv_files(directory_path, patterns, all_files)

  if (length(file_paths) == 0L) {
    if (!quiet) cli::cli_alert_warning("No files matched the supplied patterns")
    return(if (combine) data.frame() else list())
  }

  if (!quiet) {
    cli::cli_alert_info("Reading {length(file_paths)} file{?s}")
  }

  df_list <- .load_csv_files(file_paths, missing_vals, clean_col_names, quiet)

  if (combine) {
    if (!quiet) cli::cli_alert_success("Combined {length(df_list)} file{?s}")
    return(dplyr::bind_rows(df_list))
  }

  names(df_list) <- basename(file_paths)
  if (!quiet) cli::cli_alert_success("Returning list of {length(df_list)} data frame{?s}")
  df_list
}

#' Find CSV files matching the supplied patterns.
#' @keywords internal
#' @noRd
.find_csv_files <- function(directory_path, patterns, all_files) {
  if (all_files) {
    return(list.files(directory_path, pattern = "\\.csv$", full.names = TRUE))
  }
  regex <- paste0(".*(?:", paste(patterns, collapse = "|"), ").*\\.csv$")
  unique(list.files(directory_path, pattern = regex, full.names = TRUE))
}

#' Load CSV files into a list of data frames, with optional progress.
#' @keywords internal
#' @noRd
.load_csv_files <- function(file_paths, missing_vals, clean_col_names, quiet) {
  n <- length(file_paths)
  show_bar <- !quiet && n > 10L

  if (show_bar) {
    cli::cli_progress_bar("Reading files", total = n)
  }

  df_list <- vector("list", n)
  for (i in seq_len(n)) {
    file_path <- file_paths[i]
    file_name <- basename(file_path)

    if (show_bar) {
      cli::cli_progress_update()
    } else if (!quiet) {
      cli::cli_alert("Reading {i}/{n}: {.file {file_name}}")
    }

    df_list[[i]] <- tryCatch(
      utils::read.csv(
        file             = file_path,
        na.strings       = missing_vals,
        stringsAsFactors = FALSE
      ),
      error = function(e) {
        cli::cli_abort(c(
          "Failed to read {.file {file_name}}",
          "x" = conditionMessage(e)
        ))
      }
    )

    if (clean_col_names) {
      df_list[[i]] <- janitor::clean_names(df_list[[i]])
    }
  }

  if (show_bar) cli::cli_progress_done()
  df_list
}
