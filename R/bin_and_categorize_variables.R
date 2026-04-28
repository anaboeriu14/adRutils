#' Bin and categorize variables into groups
#'
#' Transforms variables into categorical groups using cutpoints (continuous),
#' value mappings (categorical), or custom functions.
#'
#' @param dataf A data frame.
#' @param groups A list of group specifications. Each element is itself a list
#'   with these fields:
#'
#'   * `col` (required): Name of the column in `dataf` to transform.
#'   * `type` (required): One of `"cutpoints"`, `"categorical"`, or `"custom"`.
#'   * `name` (optional): Output column name. Defaults to `<col>_group`.
#'   * `cutpoints` (required for `type = "cutpoints"`): Numeric vector of
#'     internal break points, in increasing order. Intervals are
#'     left-open, right-closed, except the leftmost which is closed on
#'     both ends (matching [base::cut()] with `include.lowest = TRUE`).
#'   * `labels` (optional, for `type = "cutpoints"`): Character vector of
#'     length `length(cutpoints) + 1`. Defaults to interval notation
#'     derived from the cutpoints.
#'   * `values` (required for `type = "categorical"`): Named vector mapping
#'     original values to new labels. All mappings are applied
#'     simultaneously, so chained remappings will not occur.
#'   * `custom_fn` (required for `type = "custom"`): A function taking the
#'     column vector and returning a vector of the same length.
#'
#' @param filter_missing If `TRUE`, drop rows with `NA` in any newly created
#'   group column. Default `FALSE`.
#' @param quiet If `TRUE`, suppress informational messages. Default `FALSE`.
#'
#' @return `dataf` with one new column per element of `groups`.
#'
#' @examples
#' df <- data.frame(
#'   age      = c(45, 67, 72, 81, 55),
#'   sex_code = c(1, 2, 1, 2, 1),
#'   bmi      = c(22.4, 28.1, 31.5, 24.8, 35.2)
#' )
#'
#' bin_and_categorize_variables(df, groups = list(
#'   list(col = "age",      type = "cutpoints",   cutpoints = c(60, 75)),
#'   list(col = "sex_code", type = "categorical",
#'        values = c("1" = "Male", "2" = "Female"),
#'        name   = "sex"),
#'   list(col = "bmi",      type = "cutpoints",
#'        cutpoints = c(18.5, 25, 30),
#'        labels    = c("Underweight", "Normal", "Overweight", "Obese"))
#' ))
#'
#' @export
bin_and_categorize_variables <- function(dataf, groups,
                                         filter_missing = FALSE,
                                         quiet          = FALSE) {

  validate_args(
    data           = dataf,
    groups         = is_nonempty_list(),
    filter_missing = is_flag(),
    quiet          = is_flag()
  )

  # Pre-validate every group spec and resolve output names up front so we
  # can detect collisions before mutating anything.
  resolved  <- lapply(groups, .resolve_group_spec, dataf = dataf)
  out_names <- vapply(resolved, `[[`, character(1), "name")

  if (anyDuplicated(out_names)) {
    dups <- unique(out_names[duplicated(out_names)])
    cli::cli_abort(c(
      "Duplicate output column name{?s}: {.field {dups}}",
      "i" = "Set {.field name} explicitly on conflicting groups."
    ))
  }

  overwritten <- intersect(out_names, names(dataf))

  result <- dataf
  for (g in resolved) {
    result[[g$name]] <- .create_grouped_variable(result[[g$col]], g)
  }

  if (!quiet && length(overwritten) > 0L) {
    cli::cli_alert_warning(
      "Overwrote existing column{?s}: {.field {overwritten}}"
    )
  }

  if (filter_missing) {
    n_before  <- nrow(result)
    result    <- result[stats::complete.cases(result[out_names]), , drop = FALSE]
    n_dropped <- n_before - nrow(result)
    if (!quiet && n_dropped > 0L) {
      cli::cli_alert_info("Dropped {n_dropped} row{?s} with missing group values")
    }
  }

  if (!quiet) {
    cli::cli_alert_success("Created {length(out_names)} grouped variable{?s}")
  }

  result
}

#' Validate one group spec and resolve its output column name.
#' @keywords internal
#' @noRd
.resolve_group_spec <- function(group, dataf) {
  if (is.null(group$col) || is.null(group$type)) {
    cli::cli_abort("Each group must have {.field col} and {.field type}")
  }
  if (!group$col %in% names(dataf)) {
    cli::cli_abort("Column {.field {group$col}} not found in data")
  }
  if (!group$type %in% c("cutpoints", "categorical", "custom")) {
    cli::cli_abort("Unknown {.field type}: {.val {group$type}}")
  }

  switch(group$type,
         cutpoints = {
           if (is.null(group$cutpoints) || !is.numeric(group$cutpoints)) {
             cli::cli_abort("{.field cutpoints} (numeric) required for type {.val cutpoints}")
           }
           if (is.unsorted(group$cutpoints)) {
             cli::cli_abort("{.field cutpoints} must be in increasing order")
           }
         },
         categorical = {
           if (is.null(group$values) || is.null(names(group$values))) {
             cli::cli_abort("{.field values} (named vector) required for type {.val categorical}")
           }
         },
         custom = {
           if (!is.function(group$custom_fn)) {
             cli::cli_abort("{.field custom_fn} (function) required for type {.val custom}")
           }
         }
  )

  group$name <- group$name %||% paste0(group$col, "_group")
  group
}

#' Dispatch to the appropriate transformation based on group type.
#' @keywords internal
#' @noRd
.create_grouped_variable <- function(x, group) {
  switch(group$type,
         cutpoints   = .bin_by_cutpoints(x, group$cutpoints, group$labels),
         categorical = .recode_categorical(x, group$values),
         custom      = .apply_custom_function(x, group$custom_fn)
  )
}

#' @keywords internal
#' @noRd
.bin_by_cutpoints <- function(x, cutpoints, labels = NULL) {
  breaks <- c(-Inf, cutpoints, Inf)
  labels <- labels %||% .default_cutpoint_labels(cutpoints)

  if (length(labels) != length(breaks) - 1L) {
    cli::cli_abort(c(
      "Wrong number of labels.",
      "i" = "Got {length(labels)}, need {length(breaks) - 1L} ({length(cutpoints)} cutpoint{?s} -> {length(breaks) - 1L} bin{?s})."
    ))
  }
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)
}

#' Default labels matching cut()'s left-open right-closed semantics
#' (with the leftmost interval closed via include.lowest = TRUE).
#' @keywords internal
#' @noRd
.default_cutpoint_labels <- function(cutpoints) {
  k <- length(cutpoints)
  c(
    paste0("<=", cutpoints[1]),
    if (k >= 2) paste0("(", cutpoints[-k], ",", cutpoints[-1], "]"),
    paste0(">", cutpoints[k])
  )
}

#' Single-pass mapping; avoids order-dependent chained remaps.
#' @keywords internal
#' @noRd
.recode_categorical <- function(x, value_map) {
  x_chr   <- as.character(x)
  out     <- x_chr
  matched <- match(x_chr, names(value_map))
  hit     <- !is.na(matched)
  out[hit] <- unname(value_map[matched[hit]])
  factor(out)
}

#' @keywords internal
#' @noRd
.apply_custom_function <- function(x, custom_fn) {
  out <- custom_fn(x)
  if (length(out) != length(x)) {
    cli::cli_abort(c(
      "{.field custom_fn} must return a vector the same length as input.",
      "i" = "Got length {length(out)}, expected {length(x)}."
    ))
  }
  if (is.character(out)) out <- factor(out)
  out
}
