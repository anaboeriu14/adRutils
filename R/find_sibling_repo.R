#' Find Related Repository
#'
#' Locate a sibling repository in the same parent directory as the current project.
#' Useful for multi-repo projects where one repo depends on data/code from another.
#'
#' @param repo_name Character. Primary name of the repository to find.
#' @param alternative_names Character vector. Alternative names to search for if primary name not found.
#' @param parent_levels Integer. How many levels up to go to find parent directory (default: 1).
#' @param current_repo_name Character. Name of current repository for error messages (optional).
#' @param stop_on_missing Logical. Whether to stop with error if repo not found (default: TRUE).
#'
#' @return Character path to the found repository, or NULL if not found and stop_on_missing = FALSE.
#'
#' @examples
#' \dontrun{
#' # Find apoeDatabase repo from apoeAncestry
#' db_path <- find_sibling_repo("apoeDatabase",
#'                              c("apoeD", "apoe-database", "apoe_database"))
#'
#' # Find data repo with custom error message
#' data_path <- find_sibling_repo("myproject-data",
#'                                current_repo_name = "myproject-analysis")
#'
#' # Don't error if not found
#' optional_repo <- find_sibling_repo("optional-repo", stop_on_missing = FALSE)
#' }
#'
#' @export
find_sibling_repo <- function(repo_name,
                              alternative_names = NULL,
                              parent_levels = 1,
                              current_repo_name = NULL,
                              stop_on_missing = TRUE) {

  # Get current working directory
  current_dir <- getwd()

  # Go up specified number of levels to parent directory
  parent_dir <- current_dir
  for (i in seq_len(parent_levels)) {
    parent_dir <- dirname(parent_dir)
  }

  # Look for primary repo name
  target_path <- file.path(parent_dir, repo_name)

  if (dir.exists(target_path)) {
    return(target_path)
  }

  # Try alternative names if provided
  if (!is.null(alternative_names)) {
    for (alt_name in alternative_names) {
      alt_path <- file.path(parent_dir, alt_name)
      if (dir.exists(alt_path)) {
        cli::cli_alert_info("Found {repo_name} at: {alt_path}")
        return(alt_path)
      }
    }
  }

  # Handle not found
  if (!stop_on_missing) {
    return(NULL)
  }

  # Build error message
  searched_paths <- c(target_path)
  if (!is.null(alternative_names)) {
    searched_paths <- c(searched_paths, file.path(parent_dir, alternative_names))
  }

  current_repo_msg <- if (!is.null(current_repo_name)) {
    paste0(" from ", current_repo_name)
  } else {
    ""
  }
  cli_abort(c(
    "{repo_name} repository not found{current_repo_msg}",
    "x" = "Searched for:",
    " " = searched_paths,
    "i" = "Expected structure: parent_folder/{repo_name}{if (!is.null(current_repo_name)) paste0(' and parent_folder/', current_repo_name) else ''}"
  ))
}

