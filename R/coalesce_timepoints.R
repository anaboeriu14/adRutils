#' Coalesce measures across longitudinal timepoints
#'
#' For each base variable, selects the first non-missing value across timepoints
#' in priority order. Creates `*_final` and `*_source` columns.
#'
#' @param data A data frame.
#' @param base_names Character vector of variable base names (e.g., `"dsst"`, `"ravlt"`).
#' @param timepoints Character vector of suffixes in priority order (first = preferred).
#' @param final_suffix Suffix for coalesced column. Default `"_final"`.
#' @param source_suffix Suffix for source-tracking column. Default `"_source"`.
#' @param timepoint_labels Optional named vector mapping suffixes to display labels.
#'   If `NULL`, suffixes are used as-is.
#'
#' @return The input data with added `*_final` and `*_source` columns.
#'
#' @examples
#' \dontrun{
#' data <- coalesce_timepoints(
#'   data,
#'   base_names = c("dsst", "ravlt", "moca"),
#'   timepoints = c("35", "30")
#' )
#' }
#'
#' @export
coalesce_timepoints <- function(data,
                                base_names,
                                timepoints,
                                final_suffix = "_final",
                                source_suffix = "_source",
                                timepoint_labels = NULL) {

  validate_params(
    data = data,
    custom_checks = list(
      list(
        condition = length(timepoints) >= 2,
        message = "{.arg timepoints} must have at least 2 elements."
      ),
      list(
        condition = is.character(base_names) && length(base_names) > 0,
        message = "{.arg base_names} must be a non-empty character vector."
      )
    ),
    context = "coalesce_timepoints"
  )

  if (is.null(timepoint_labels)) {
    timepoint_labels <- stats::setNames(timepoints, timepoints)
  }

  result <- data

  for (base_name in base_names) {

    tp_cols <- paste0(base_name, timepoints)
    existing <- tp_cols[tp_cols %in% names(result)]

    if (length(existing) == 0) next

    final_col <- paste0(base_name, final_suffix)
    source_col <- paste0(base_name, source_suffix)

    # Coalesce in priority order
    result <- result %>%
      dplyr::mutate(!!final_col := dplyr::coalesce(!!!rlang::syms(existing)))

    # Track which timepoint contributed
    source_expr <- purrr::map(existing, function(col) {
      suffix <- sub(paste0("^", base_name), "", col)
      label <- timepoint_labels[suffix] %||% suffix
      rlang::expr(!is.na(!!rlang::sym(col)) ~ !!label)
    })
    source_expr <- append(source_expr, list(rlang::expr(TRUE ~ NA_character_)))

    result <- result %>%
      dplyr::mutate(!!source_col := dplyr::case_when(!!!source_expr))
  }

  return(result)
}


