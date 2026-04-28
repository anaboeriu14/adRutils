#' Locate a sibling repository's path
#'
#' Finds a repository that shares a parent directory with the current
#' project and returns its absolute path. Uses [here::here()] to determine
#' the project root, so the result is independent of the working directory.
#'
#' @param repo_name Primary directory name to find.
#' @param alternative_names Character vector of fallback names to try if
#'   `repo_name` is not found. Optional.
#' @param parent_levels Number of directory levels to ascend from the
#'   project root before searching. Default `1` (find an immediate sibling).
#' @param current_repo_name Name of the calling repository, used in error
#'   messages for clarity. Defaults to the basename of [here::here()].
#' @param stop_on_missing If `TRUE` (default), abort when nothing is found.
#'   If `FALSE`, return `NULL` instead.
#'
#' @return The absolute path to the found repository, or `NULL` if not
#'   found and `stop_on_missing = FALSE`.
#'
#' @examples
#' \dontrun{
#' # Find a sibling repo
#' db_path <- find_sibling_repo_path("apoeDatabase")
#'
#' # With alternatives if naming conventions vary
#' db_path <- find_sibling_repo_path(
#'   "apoeDatabase",
#'   alternative_names = c("apoe-database", "apoe_db")
#' )
#'
#' # Soft lookup
#' optional <- find_sibling_repo_path("optional-data", stop_on_missing = FALSE)
#' }
#'
#' @export
find_sibling_repo_path <- function(repo_name,
                                   alternative_names = NULL,
                                   parent_levels     = 1,
                                   current_repo_name = basename(here::here()),
                                   stop_on_missing   = TRUE) {

  validate_args(
    repo_name         = is_string(),
    parent_levels     = is_number(min = 1),
    current_repo_name = is_string(),
    stop_on_missing   = is_flag(),
    custom_checks = list(
      list(
        condition = is.null(alternative_names) || is.character(alternative_names),
        message   = "{.arg alternative_names} must be NULL or a character vector"
      )
    )
  )

  parent_dir <- .get_parent_directory(parent_levels)
  repo_path  <- .search_for_repo(parent_dir, repo_name, alternative_names)

  if (!is.null(repo_path)) return(repo_path)

  if (stop_on_missing) {
    .abort_repo_not_found(parent_dir, repo_name, alternative_names, current_repo_name)
  }
  NULL
}

#' Walk up `parent_levels` directories from the project root.
#' @keywords internal
#' @noRd
.get_parent_directory <- function(parent_levels) {
  parent_dir <- here::here()
  for (i in seq_len(parent_levels)) {
    parent_dir <- dirname(parent_dir)
  }
  parent_dir
}

#' Try the primary name, then any alternatives.
#' @keywords internal
#' @noRd
.search_for_repo <- function(parent_dir, repo_name, alternative_names) {
  primary <- file.path(parent_dir, repo_name)
  if (dir.exists(primary)) return(primary)

  for (alt in alternative_names) {
    alt_path <- file.path(parent_dir, alt)
    if (dir.exists(alt_path)) {
      cli::cli_alert_info("Found {.val {repo_name}} at: {.path {alt_path}}")
      return(alt_path)
    }
  }
  NULL
}

#' Build a clear error listing every path that was searched.
#' @keywords internal
#' @noRd
.abort_repo_not_found <- function(parent_dir, repo_name, alternative_names,
                                  current_repo_name) {
  searched <- c(
    file.path(parent_dir, repo_name),
    if (!is.null(alternative_names)) file.path(parent_dir, alternative_names)
  )

  cli::cli_abort(c(
    "{.val {repo_name}} repository not found from {.val {current_repo_name}}",
    "x" = "Searched:",
    stats::setNames(searched, rep("*", length(searched))),
    "i" = "Expected layout: {.path parent/{repo_name}} alongside {.path parent/{current_repo_name}}"
  ))
}
