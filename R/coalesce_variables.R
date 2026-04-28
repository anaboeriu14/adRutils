#' Coalesce related variables into a single column
#'
#' Combines groups of columns into one column per group by taking the first
#' non-missing value across them (using [dplyr::coalesce()]). Groups can be
#' specified manually via `var_groups` or discovered automatically via a
#' regex `pattern_extract` that identifies the varying portion of column
#' names.
#'
#' @param dataf A data frame containing the variables to coalesce.
#' @param pattern_extract Regex matching the varying segment of column names.
#'   Columns are grouped by the *non-matching* portion of their name.
#'   Mutually exclusive with `var_groups`.
#' @param var_groups Named list of column groups. Each name becomes the new
#'   coalesced column name. Mutually exclusive with `pattern_extract`.
#' @param prefix String to prepend to new column names. Default `""`.
#' @param overwrite If `TRUE`, replace existing columns; if `FALSE` (default),
#'   skip them with a warning.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `dataf` with coalesced columns added.
#'
#' @examples
#' df <- data.frame(
#'   bmi_v1 = c(22, NA, 28),
#'   bmi_v2 = c(NA, 25, 27),
#'   age_v1 = c(45, NA, 70),
#'   age_v2 = c(NA, 60, NA)
#' )
#'
#' # Pattern-based: strip "_v[12]" from each name and group by the rest
#' coalesce_variables(df, pattern_extract = "_v[12]")
#'
#' # Manual grouping
#' coalesce_variables(df, var_groups = list(
#'   bmi = c("bmi_v1", "bmi_v2"),
#'   age = c("age_v1", "age_v2")
#' ))
#'
#' @export
coalesce_variables <- function(dataf,
                               pattern_extract = NULL,
                               var_groups      = NULL,
                               prefix          = "",
                               overwrite       = FALSE,
                               quiet           = FALSE) {

  validate_args(
    data      = dataf,
    overwrite = is_flag(),
    quiet     = is_flag(),
    custom_checks = list(
      list(
        condition = !is.null(pattern_extract) || !is.null(var_groups),
        message   = "Either {.arg pattern_extract} or {.arg var_groups} must be provided"
      ),
      list(
        condition = is.null(pattern_extract) || is.null(var_groups),
        message   = "Provide {.arg pattern_extract} OR {.arg var_groups}, not both"
      ),
      list(
        condition = is.null(pattern_extract) ||
          (is.character(pattern_extract) &&
             length(pattern_extract) == 1L &&
             nzchar(pattern_extract)),
        message   = "{.arg pattern_extract} must be a single non-empty string"
      ),
      list(
        condition = is.null(var_groups) ||
          (is.list(var_groups) && !is.null(names(var_groups))),
        message   = "{.arg var_groups} must be a named list"
      ),
      list(
        condition = is.character(prefix) && length(prefix) == 1L,
        message   = "{.arg prefix} must be a single string"
      )
    )
  )

  groups <- if (!is.null(pattern_extract)) {
    .build_groups_from_pattern(dataf, pattern_extract)
  } else {
    validate_args(data = dataf, columns = unlist(var_groups))
    var_groups
  }

  if (length(groups) == 0L) {
    if (!quiet) cli::cli_alert_warning("No matching columns found for coalescing")
    return(dataf)
  }

  .coalesce_groups(dataf, groups, prefix, overwrite, quiet)
}

#' Build column groups from a regex pattern.
#' @keywords internal
#' @noRd
.build_groups_from_pattern <- function(dataf, pattern_extract) {
  matching_cols <- grep(pattern_extract, names(dataf), value = TRUE)
  if (length(matching_cols) == 0L) return(list())

  groups <- list()
  for (col in matching_cols) {
    parts <- stringr::str_split(col, pattern_extract, n = 2L)[[1]]
    if (length(parts) == 2L) {
      base_name <- paste0(parts[1], parts[2])
      groups[[base_name]] <- c(groups[[base_name]], col)
    }
  }
  groups
}

#' Apply coalesce() per group, tracking what was created/overwritten/skipped.
#' @keywords internal
#' @noRd
.coalesce_groups <- function(dataf, groups, prefix, overwrite, quiet) {
  result      <- dataf
  created     <- character()
  overwritten <- character()
  skipped     <- character()
  singletons  <- character()

  for (base_name in names(groups)) {
    cols <- groups[[base_name]]

    if (length(cols) < 2L) {
      singletons <- c(singletons, base_name)
      next
    }

    new_name <- paste0(prefix, base_name)

    if (new_name %in% names(result)) {
      if (!overwrite) {
        skipped <- c(skipped, new_name)
        next
      }
      overwritten <- c(overwritten, new_name)
    } else {
      created <- c(created, new_name)
    }

    result[[new_name]] <- do.call(dplyr::coalesce, as.list(result[cols]))
  }

  if (!quiet) {
    if (length(singletons) > 0L) {
      cli::cli_alert_info(
        "Skipped {length(singletons)} singleton group{?s} (only one matching column): {.field {singletons}}"
      )
    }
    if (length(skipped) > 0L) {
      cli::cli_alert_warning(
        "Skipped existing column{?s}: {.field {skipped}}"
      )
      cli::cli_alert_info("Use {.code overwrite = TRUE} to replace.")
    }
    if (length(overwritten) > 0L) {
      cli::cli_alert_warning(
        "Overwrote existing column{?s}: {.field {overwritten}}"
      )
    }
    n_total <- length(created) + length(overwritten)
    if (n_total > 0L) {
      msg <- if (nzchar(prefix)) glue::glue(' with prefix "{prefix}"') else ""
      cli::cli_alert_success("Created {n_total} coalesced column{?s}{msg}")
    }
  }

  result
}
