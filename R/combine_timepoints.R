#' Combine measures across longitudinal visits
#'
#' For each base variable, selects the first non-missing value across a
#' priority-ordered sequence of timepoint suffixes. Creates two columns per
#' base variable: a coalesced value and a source label indicating which
#' timepoint contributed.
#'
#' @param data A data frame.
#' @param base_names Character vector of variable base names (e.g., `"dsst"`,
#'   `"ravlt"`).
#' @param timepoints Character vector of suffixes in priority order; the
#'   first entry is the preferred source.
#' @param final_suffix Suffix for the coalesced value column. Default
#'   `"_final"`.
#' @param source_suffix Suffix for the source-tracking column. Default
#'   `"_source"`.
#' @param timepoint_labels Optional named character vector mapping suffixes
#'   to display labels. If `NULL`, suffixes are used verbatim.
#'
#' @return `data` with `<base>_final` and `<base>_source` columns added for
#'   each base name that has at least one matching timepoint column.
#'
#' @examples
#' df <- data.frame(
#'   dsst30  = c(45, NA, 38),
#'   dsst35  = c(NA, 52, 41),
#'   ravlt30 = c(NA, 30, NA),
#'   ravlt35 = c(28, 32, 35)
#' )
#' combine_timepoints(
#'   df,
#'   base_names = c("dsst", "ravlt"),
#'   timepoints = c("30", "35")
#' )
#'
#' @export
combine_timepoints <- function(data,
                               base_names,
                               timepoints,
                               final_suffix     = "_final",
                               source_suffix    = "_source",
                               timepoint_labels = NULL) {

  validate_args(
    data          = data,
    base_names    = is_nonempty_character(),
    timepoints    = is_nonempty_character(),
    final_suffix  = is_string(),
    source_suffix = is_string(),
    custom_checks = list(
      list(
        condition = length(timepoints) >= 2L,
        message   = "{.arg timepoints} must have at least 2 entries"
      ),
      list(
        condition = is.null(timepoint_labels) ||
          (is.character(timepoint_labels) &&
             !is.null(names(timepoint_labels))),
        message   = "{.arg timepoint_labels} must be NULL or a named character vector"
      )
    )
  )

  if (is.null(timepoint_labels)) {
    timepoint_labels <- stats::setNames(timepoints, timepoints)
  }

  result <- data

  for (base_name in base_names) {
    tp_cols  <- paste0(base_name, timepoints)
    existing <- tp_cols[tp_cols %in% names(result)]

    if (length(existing) == 0L) next

    final_col  <- paste0(base_name, final_suffix)
    source_col <- paste0(base_name, source_suffix)

    result[[final_col]] <- do.call(
      dplyr::coalesce,
      as.list(result[existing])
    )

    # Build a source label per row by walking the priority order: for each
    # column in turn, fill in the source label only where it isn't already set.
    src <- rep(NA_character_, nrow(result))
    for (col in existing) {
      suffix <- sub(paste0("^", base_name), "", col)
      label  <- timepoint_labels[suffix] %||% suffix
      mask   <- is.na(src) & !is.na(result[[col]])
      src[mask] <- label
    }
    result[[source_col]] <- src
  }

  result
}
