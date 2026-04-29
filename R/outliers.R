#' Detect outlier thresholds via the IQR rule
#'
#' Computes lower and upper outlier bounds for a numeric variable using
#' the IQR rule: `Q1 - k*IQR` and `Q3 + k*IQR`, with `k = multiplier`.
#'
#' @param dataf A data frame.
#' @param var_name Name of a single numeric column in `dataf`.
#' @param multiplier IQR multiplier. Default `1.5` (Tukey's classic
#'   threshold); use `3` for "extreme" outliers.
#' @param label Optional label for the `notes` column. Defaults to
#'   `var_name`.
#'
#' @return A one-row data frame with columns `LB` (lower bound), `UB`
#'   (upper bound), and `notes`.
#'
#' @examples
#' df <- data.frame(x = c(1:10, 100))
#' detect_outlier_thresholds(df, "x")
#' detect_outlier_thresholds(df, "x", multiplier = 3)
#'
#' @export
detect_outlier_thresholds <- function(dataf, var_name,
                                      multiplier = 1.5,
                                      label      = NULL) {

  validate_args(
    data            = dataf,
    columns         = var_name,
    numeric_columns = var_name,
    var_name        = is_string(),
    multiplier      = is_number(positive = TRUE),
    custom_checks = list(
      list(
        condition = is.null(label) ||
          (is.character(label) && length(label) == 1L),
        message   = "{.arg label} must be NULL or a single string"
      )
    )
  )

  q1  <- stats::quantile(dataf[[var_name]], 0.25, na.rm = TRUE)
  q3  <- stats::quantile(dataf[[var_name]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  data.frame(
    LB               = q1 - multiplier * iqr,
    UB               = q3 + multiplier * iqr,
    notes            = label %||% var_name,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
}


#' Replace outliers in numeric variables with `NA`
#'
#' For each variable in `var_names`, identifies outliers using the IQR
#' rule (see [detect_outlier_thresholds()]) and replaces them with `NA`.
#' Optionally also nullifies values in paired columns whenever an outlier
#' is detected in the source column.
#'
#' This function is idempotent: running it twice produces the same result
#' as running it once, since outliers are replaced with `NA` and `NA`
#' values are skipped on subsequent passes.
#'
#' @param dataf A data frame.
#' @param var_names Character vector of numeric column names to check.
#'   Non-numeric columns in this list are skipped with a warning.
#' @param multiplier IQR multiplier. Default `1.5`.
#' @param paired_cols Optional named list mapping a source column to a
#'   paired column. When an outlier is detected in the source column,
#'   the same row in the paired column is also set to `NA`. For example,
#'   `list(weight = "log_weight")` ensures `log_weight` is set to `NA`
#'   wherever `weight` was an outlier.
#' @param remove_all_na_rows If `TRUE` (default), drop rows where every
#'   variable in `var_names` is `NA` after outlier replacement.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `dataf` with outliers replaced by `NA` (and possibly fewer
#'   rows if `remove_all_na_rows = TRUE`).
#'
#' @examples
#' df <- data.frame(
#'   weight     = c(70, 72, 68, 200, 71),
#'   log_weight = c(log(70), log(72), log(68), log(200), log(71))
#' )
#' replace_outliers_with_na(
#'   df,
#'   var_names   = "weight",
#'   paired_cols = list(weight = "log_weight")
#' )
#'
#' @export
replace_outliers_with_na <- function(dataf, var_names,
                                     multiplier         = 1.5,
                                     paired_cols        = NULL,
                                     remove_all_na_rows = TRUE,
                                     quiet              = FALSE) {

  validate_args(
    data               = dataf,
    columns            = var_names,
    var_names          = is_nonempty_character(),
    multiplier         = is_number(positive = TRUE),
    remove_all_na_rows = is_flag(),
    quiet              = is_flag(),
    custom_checks = list(
      list(
        condition = is.null(paired_cols) ||
          (is.list(paired_cols) && !is.null(names(paired_cols))),
        message   = "{.arg paired_cols} must be NULL or a named list"
      )
    )
  )

  result         <- dataf
  total_outliers <- 0L

  for (var in var_names) {
    if (!is.numeric(result[[var]])) {
      if (!quiet) cli::cli_alert_warning("Skipping non-numeric column: {.field {var}}")
      next
    }

    step <- .replace_outliers_single_var(result, var, multiplier, paired_cols)
    result         <- step$data
    total_outliers <- total_outliers + step$n_outliers
  }

  if (remove_all_na_rows) {
    result <- .remove_all_na_rows(result, dataf, var_names, quiet)
  }

  if (!quiet) {
    if (total_outliers > 0L) {
      cli::cli_alert_success(
        "Replaced {total_outliers} outlier{?s} with NA across {length(var_names)} variable{?s}"
      )
    } else {
      cli::cli_alert_info("No outliers detected")
    }
  }

  result
}


#' Replace outliers for a single variable, optionally nullifying a
#' paired column at the same row positions.
#' @keywords internal
#' @noRd
.replace_outliers_single_var <- function(dataf, var, multiplier, paired_cols) {
  cutoffs <- detect_outlier_thresholds(dataf, var, multiplier)

  is_outlier <- !is.na(dataf[[var]]) &
    (dataf[[var]] < cutoffs$LB | dataf[[var]] > cutoffs$UB)
  n_outliers <- sum(is_outlier)

  if (n_outliers > 0L) {
    dataf[is_outlier, var] <- NA

    if (!is.null(paired_cols) && var %in% names(paired_cols)) {
      paired <- paired_cols[[var]]
      if (paired %in% names(dataf)) {
        dataf[is_outlier, paired] <- NA
      }
    }
  }

  list(data = dataf, n_outliers = n_outliers)
}


#' Drop rows where every variable in `var_names` is NA after outlier
#' replacement.
#' @keywords internal
#' @noRd
.remove_all_na_rows <- function(result, original, var_names, quiet) {
  all_na    <- apply(result[var_names], 1, function(row) all(is.na(row)))
  n_removed <- sum(all_na)

  if (n_removed > 0L) {
    result <- result[!all_na, , drop = FALSE]
    if (!quiet) {
      pct <- round(100 * n_removed / nrow(original), 1)
      cli::cli_alert_info("Removed {n_removed} all-NA row{?s} ({pct}%)")
    }
  }

  result
}
