#' Find Related Repository
#'
#' Locate a sibling repository in the same parent directory as the current project.
#' Uses the `here` package to reliably find the project root, ensuring consistent
#' behavior regardless of working directory.
#'
#' @param repo_name Character. Primary name of the repository to find.
#' @param alternative_names Character vector. Alternative names to search for if
#'   primary name not found (optional).
#' @param parent_levels Integer. How many directory levels to go up from the
#'   project root to find siblings (default: 1).
#' @param current_repo_name Character. Name of current repository, used in error
#'   messages for clarity (optional).
#' @param stop_on_missing Logical. If TRUE, throws error when repo not found.
#'   If FALSE, returns NULL silently (default: TRUE).
#'
#' @return Character path to the found repository, or NULL if not found and
#'   stop_on_missing = FALSE.
#'
#' @examples
#' \dontrun{
#' # Basic usage - find sibling repo
#' db_path <- find_sibling_repo("apoeDatabase")
#'
#' # With alternatives if repo name varies
#' db_path <- find_sibling_repo(
#'   "apoeDatabase",
#'   alternative_names = c("apoe-database", "apoe_db")
#' )
#'
#' # Non-failing version
#' optional_repo <- find_sibling_repo("optional-data", stop_on_missing = FALSE)
#' if (!is.null(optional_repo)) {
#'   # Use the repo
#' }
#' }
#'
#' @export
find_sibling_repo <- function(repo_name, alternative_names = NULL,
                              parent_levels = 1, current_repo_name = NULL,
                              stop_on_missing = TRUE) {
  # Validate inputs
  validate_params(
    custom_checks = list(
      list(
        condition = is.character(repo_name) && length(repo_name) == 1 && nchar(repo_name) > 0,
        message = "{.arg repo_name} must be a non-empty character string"
      ),
      list(
        condition = is.null(alternative_names) || is.character(alternative_names),
        message = "{.arg alternative_names} must be NULL or a character vector"
      ),
      list(
        condition = is.numeric(parent_levels) && parent_levels >= 1,
        message = "{.arg parent_levels} must be a positive integer"
      ),
      list(
        condition = is.logical(stop_on_missing) && length(stop_on_missing) == 1,
        message = "{.arg stop_on_missing} must be a single logical value"
      )
    ),
    context = "find_sibling_repo"
  )

  # Get parent directory (using here for project root)
  parent_dir <- .get_parent_directory(parent_levels)

  # Search for repository
  repo_path <- .search_for_repo(parent_dir, repo_name, alternative_names)

  # Handle result
  if (!is.null(repo_path)) {
    return(repo_path)
  }

  # Repository not found
  if (stop_on_missing) {
    .abort_repo_not_found(parent_dir, repo_name, alternative_names, current_repo_name)
  }

  return(NULL)
}

#' Navigate to parent directory from project root
#' @keywords internal
#' @noRd
.get_parent_directory <- function(parent_levels) {
  # Start from project root
  project_root <- here::here()

  # copy without modifying project root
  parent_dir <- project_root

  # Navigate up the specified number of levels
  for (i in seq_len(parent_levels)) {
    parent_dir <- dirname(parent_dir)
  }

  return(parent_dir)
}

#' Search for repository by name and alternatives
#' @keywords internal
#' @noRd
.search_for_repo <- function(parent_dir, repo_name, alternative_names) {
  # Try primary name
  target_path <- file.path(parent_dir, repo_name)

  if (dir.exists(target_path)) {
    return(target_path)
  }

  # Try alternative names
  if (!is.null(alternative_names)) {
    for (alt_name in alternative_names) {
      alt_path <- file.path(parent_dir, alt_name)

      if (dir.exists(alt_path)) {
        cli::cli_alert_info("Found {repo_name} at: {alt_path}")
        return(alt_path)
      }
    }
  }

  return(NULL)
}

#' Build and throw error for repository not found
#' @keywords internal
#' @noRd
.abort_repo_not_found <- function(parent_dir, repo_name, alternative_names,
                                  current_repo_name) {
  # Build list of searched paths
  searched_paths <- file.path(parent_dir, repo_name)

  if (!is.null(alternative_names)) {
    alt_paths <- file.path(parent_dir, alternative_names)
    searched_paths <- c(searched_paths, alt_paths)
  }

  # Build context message
  context_msg <- if (!is.null(current_repo_name)) {
    paste0(" from ", current_repo_name)
  } else {
    ""
  }

  # Build expected structure hint
  expected_hint <- if (!is.null(current_repo_name)) {
    paste0("parent_folder/", repo_name, " and parent_folder/", current_repo_name)
  } else {
    paste0("parent_folder/", repo_name)
  }

  # Throw error
  cli::cli_abort(c(
    "{repo_name} repository not found{context_msg}",
    "x" = "Searched for:",
    " " = searched_paths,
    "i" = "Expected structure: {expected_hint}"
  ))
}
