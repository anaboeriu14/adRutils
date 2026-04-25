# ==============================================================================
# META-ANALYSIS UTILITIES
# ------------------------------------------------------------------------------
# Generic helpers for discovery + replication (or multi-cohort) meta-analyses
# on tidy effect-size dataframes.
#
# Typical pipeline:
#   check_cohort_alignment(discovery, replication, by = c("outcome", "term"))
#   bind_rows(discovery, replication) |>
#     add_inverse_variance_weights(group_vars = c("outcome", "term")) |>
#     add_meta_pooled_results(group_vars = c("outcome", "term"))
# ==============================================================================

#' Check that two dataframes have matching key combinations
#'
#' Compares the set of key combinations between two dataframes (typically a
#' discovery and replication cohort) and reports any combinations present in
#' only one. Useful as a pre-flight check before joining or meta-analyzing.
#'
#' @param x,y Dataframes to compare.
#' @param by Character vector of column names defining the key.
#' @param x_label,y_label Labels used in messages (default "x" and "y").
#' @param quiet If `TRUE`, suppress CLI output. Default `FALSE`.
#'
#' @return Invisibly, a list with elements `x_only`, `y_only`, and
#'   `perfect_match` (logical).
#' @export
check_cohort_alignment <- function(x, y, by,
                                   x_label = "x", y_label = "y",
                                   quiet = FALSE) {

  stopifnot(is.data.frame(x), is.data.frame(y), is.character(by))

  missing_x <- setdiff(by, names(x))
  missing_y <- setdiff(by, names(y))
  if (length(missing_x) > 0) {
    cli::cli_abort("Columns missing from {x_label}: {.val {missing_x}}")
  }
  if (length(missing_y) > 0) {
    cli::cli_abort("Columns missing from {y_label}: {.val {missing_y}}")
  }

  x_combos <- dplyr::distinct(x, dplyr::across(dplyr::all_of(by)))
  y_combos <- dplyr::distinct(y, dplyr::across(dplyr::all_of(by)))

  x_only <- dplyr::anti_join(x_combos, y_combos, by = by)
  y_only <- dplyr::anti_join(y_combos, x_combos, by = by)

  perfect_match <- nrow(x_only) == 0 && nrow(y_only) == 0

  if (!quiet) {
    if (nrow(x_only) > 0) {
      cli::cli_alert_warning(
        "{nrow(x_only)} combination{?s} only in {x_label}"
      )
    }
    if (nrow(y_only) > 0) {
      cli::cli_alert_warning(
        "{nrow(y_only)} combination{?s} only in {y_label}"
      )
    }
    if (perfect_match) {
      cli::cli_alert_success("Perfect alignment on {.val {by}}")
    }
  }

  invisible(list(
    x_only = x_only,
    y_only = y_only,
    perfect_match = perfect_match
  ))
}


#' Add inverse-variance weights to a tidy effects dataframe
#'
#' Computes per-row inverse-variance weights (1 / SE^2), normalized to sum to 1
#' within each group. Rows with missing, zero, or negative standard errors are
#' dropped with a warning, since they cannot be validly weighted.
#'
#' @param data A dataframe with at least `std.error` column.
#' @param group_vars Character vector of columns defining groups within which
#'   weights should sum to 1 (e.g., `c("outcome", "term")`).
#' @param se_col Name of the standard error column. Default `"std.error"`.
#'
#' @return `data` with an added `weight` column, minus any rows with invalid SE.
#' @export
add_inverse_variance_weights <- function(data,
                                         group_vars,
                                         se_col = "std.error") {

  stopifnot(is.data.frame(data), is.character(group_vars))

  if (!se_col %in% names(data)) {
    cli::cli_abort("Column {.val {se_col}} not found in data.")
  }

  missing_groups <- setdiff(group_vars, names(data))
  if (length(missing_groups) > 0) {
    cli::cli_abort("Group column{?s} not found: {.val {missing_groups}}")
  }

  se <- data[[se_col]]
  invalid <- is.na(se) | se <= 0

  if (any(invalid)) {
    n_na   <- sum(is.na(se))
    n_zero <- sum(se == 0, na.rm = TRUE)
    n_neg  <- sum(se < 0,  na.rm = TRUE)

    cli::cli_warn(c(
      "Dropping {sum(invalid)} row{?s} with invalid {.val {se_col}}:",
      "i" = "NA: {n_na}, zero: {n_zero}, negative: {n_neg}"
    ))

    data <- data[!invalid, , drop = FALSE]
  }

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate(
      inv_var = 1 / .data[[se_col]]^2,
      weight   = .data$inv_var / sum(.data$inv_var)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"inv_var")
}


#' Run REML meta-analysis within groups and append pooled rows
#'
#' For each group defined by `group_vars`, fits a random-effects meta-analysis
#' via `metafor::rma(method = "REML")` and appends a pooled row to the data.
#' Heterogeneity statistics (I², Q, τ²) are computed when k ≥ 2.
#'
#' When exactly 2 cohorts are present in a group, two replication-style
#' indicators are also computed: `same_direction` (sign agreement) and
#' `replicated` (sign agreement AND |β/SE| > 1.96 in the second cohort). These
#' concepts do not generalize cleanly to >2 cohorts, so they are set to NA
#' when k ≥ 3.
#'
#' Groups with k = 1 after any upstream filtering are retained as-is, with no
#' pooled row appended (and a warning).
#'
#' @param data A tidy effects dataframe containing at minimum `cohort`,
#'   `estimate`, `std.error`, and `n_obs` columns.
#' @param group_vars Character vector of grouping columns (e.g.,
#'   `c("outcome", "term")`).
#' @param cohort_col Name of the cohort identifier column. Default `"cohort"`.
#' @param discovery_label,replication_label Labels identifying the discovery
#'   and replication cohorts, used only when computing `same_direction` and
#'   `replicated` for k = 2 groups. If `NULL` (default), the first and second
#'   cohort values (in appearance order) are used.
#' @param pooled_label Label used for the appended pooled row. Default
#'   `"Pooled"`.
#' @param cohort_levels Optional character vector defining factor levels for
#'   the `cohort` column in the returned data. If `NULL`, a factor is built
#'   from the unique values with `pooled_label` last.
#'
#' @return `data` with pooled rows appended, including columns `I2`, `Q`,
#'   `tau2`, `n_cohorts`, `same_direction`, and `replicated`.
#' @export
add_meta_pooled_results <- function(data,
                                    group_vars,
                                    cohort_col         = "cohort",
                                    discovery_label    = NULL,
                                    replication_label  = NULL,
                                    pooled_label       = "Pooled",
                                    cohort_levels      = NULL) {

  stopifnot(is.data.frame(data), is.character(group_vars))

  required <- c(cohort_col, "estimate", "std.error", "n_obs")
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("Column{?s} missing from data: {.val {missing}}")
  }

  # Columns to carry through from the first row of each group
  preserve_cols <- setdiff(
    names(data),
    c(cohort_col, "estimate", "std.error", "lci", "uci",
      "n_obs", "statistic", "p.value", "sig", "weight",
      "I2", "Q", "tau2", "n_cohorts",
      "same_direction", "replicated")
  )

  pooled <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::group_map(function(.x, .y) {

      k <- nrow(.x)

      if (k < 2) {
        cli::cli_warn(c(
          "Skipping pooling for group with k = {k}:",
          "i" = "{paste(names(.y), unlist(.y), sep = ' = ', collapse = ', ')}"
        ))
        return(tibble::tibble())
      }

      m <- metafor::rma(
        yi     = .x$estimate,
        sei    = .x$std.error,
        method = "REML"
      )

      # Two-cohort replication-style indicators
      same_dir <- NA
      repl     <- NA
      if (k == 2) {
        disc_lab <- discovery_label   %||% .x[[cohort_col]][1]
        repl_lab <- replication_label %||% .x[[cohort_col]][2]

        disc_est  <- .x$estimate[.x[[cohort_col]] == disc_lab][1]
        repl_est  <- .x$estimate[.x[[cohort_col]] == repl_lab][1]
        repl_se   <- .x$std.error[.x[[cohort_col]] == repl_lab][1]

        same_dir  <- sign(disc_est) == sign(repl_est)
        repl      <- isTRUE(same_dir) && abs(repl_est / repl_se) > 1.96
      }

      result <- tibble::tibble(
        !!!.y,
        !!cohort_col   := pooled_label,
        estimate       = as.numeric(m$b),
        std.error      = m$se,
        lci            = m$ci.lb,
        uci            = m$ci.ub,
        n_obs          = sum(.x$n_obs, na.rm = TRUE),
        statistic      = m$zval,
        p.value        = m$pval,
        sig            = m$pval < 0.05,
        I2             = m$I2,
        Q              = m$QE,
        tau2           = m$tau2,
        n_cohorts      = k,
        same_direction = same_dir,
        replicated     = repl,
        weight         = NA_real_
      )

      # Carry forward group-constant metadata from first row
      for (col in setdiff(preserve_cols, group_vars)) {
        if (col %in% names(.x)) result[[col]] <- .x[[col]][1]
      }
      result
    }) %>%
    dplyr::bind_rows()

  out <- dplyr::bind_rows(data, pooled)

  # Set cohort factor levels so pooled row sorts last by default
  if (is.null(cohort_levels)) {
    unique_cohorts <- setdiff(unique(out[[cohort_col]]), pooled_label)
    cohort_levels  <- c(unique_cohorts, pooled_label)
  }

  out[[cohort_col]] <- factor(out[[cohort_col]], levels = cohort_levels)

  out %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_vars)),
      .data[[cohort_col]]
    )
}
