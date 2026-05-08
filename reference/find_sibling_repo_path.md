# Locate a sibling repository's path

Finds a repository that shares a parent directory with the current
project and returns its absolute path. Uses
[`here::here()`](https://here.r-lib.org/reference/here.html) to
determine the project root, so the result is independent of the working
directory.

## Usage

``` r
find_sibling_repo_path(
  repo_name,
  alternative_names = NULL,
  parent_levels = 1,
  current_repo_name = basename(here::here()),
  stop_on_missing = TRUE
)
```

## Arguments

- repo_name:

  Primary directory name to find.

- alternative_names:

  Character vector of fallback names to try if `repo_name` is not found.
  Optional.

- parent_levels:

  Number of directory levels to ascend from the project root before
  searching. Default `1` (find an immediate sibling).

- current_repo_name:

  Name of the calling repository, used in error messages for clarity.
  Defaults to the basename of
  [`here::here()`](https://here.r-lib.org/reference/here.html).

- stop_on_missing:

  If `TRUE` (default), abort when nothing is found. If `FALSE`, return
  `NULL` instead.

## Value

The absolute path to the found repository, or `NULL` if not found and
`stop_on_missing = FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find a sibling repo
db_path <- find_sibling_repo_path("apoeDatabase")

# With alternatives if naming conventions vary
db_path <- find_sibling_repo_path(
  "apoeDatabase",
  alternative_names = c("apoe-database", "apoe_db")
)

# Soft lookup
optional <- find_sibling_repo_path("optional-data", stop_on_missing = FALSE)
} # }
```
