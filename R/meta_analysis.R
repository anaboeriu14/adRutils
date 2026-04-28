#' Check that two dataframes have matching key combinations
#'
#' Compares the set of key combinations between two dataframes (typically a
#' discovery and replication cohort) and reports any combinations present in
#' only one. Useful as a pre-flight check before joining or meta-analyzing.
#'
#' @param x,y Dataframes to compare.
#' @param by Character vector of column names defining the key.
#' @param x_label,y_label Labels used in messages. Default `"x"` and `"y"`.
#' @param quiet If `TRUE`, suppress CLI output. Default `FALSE`.
#'
#' @return Invisibly, a list with elements `x_only`, `y_only`, and
#'   `perfect_match` (logical).
#' @export
check_cohort_alignment <- function(x, y, by,
                                   x_label = "x", y_label = "y",
                                   quiet   = FALSE) {

  validate_args(
    by      = is_nonempty_character(),
    x_label = is_string(),
    y_label = is_string(),
    quiet   = is_flag(),
    custom_checks = list(
      list(
        condition = is.data.frame(x),
        message   = "{.arg x} must be a data frame"
      ),
      list(
        condition = is.data.frame(y),
        message   = "{.arg y} must be a data frame"
      )
    )
  )

  missing_x <- setdiff(by, names(x))
  missing_y <- setdiff(by, names(y))
  if (length(missing_x) > 0L) {
    cli::cli_abort("Columns missing from {x_label}: {.val {missing_x}}")
  }
  if (length(missing_y) > 0L) {
    cli::cli_abort("Columns missing from {y_label}: {.val {missing_y}}")
  }

  x_combos <- dplyr::distinct(x, dplyr::across(dplyr::all_of(by)))
  y_combos <- dplyr::distinct(y, dplyr::across(dplyr::all_of(by)))

  x_only <- dplyr::anti_join(x_combos, y_combos, by = by)
  y_only <- dplyr::anti_join(y_combos, x_combos, by = by)

  perfect_match <- nrow(x_only) == 0L && nrow(y_only) == 0L

  if (!quiet) {
    if (nrow(x_only) > 0L) {
      cli::cli_alert_warning("{nrow(x_only)} combination{?s} only in {x_label}")
    }
    if (nrow(y_only) > 0L) {
      cli::cli_alert_warning("{nrow(y_only)} combination{?s} only in {y_label}")
    }
    if (perfect_match) {
      cli::cli_alert_success("Perfect alignment on {.val {by}}")
    }
  }

  invisible(list(
    x_only        = x_only,
    y_only        = y_only,
    perfect_match = perfect_match
  ))
}


#' Run REML meta-analysis within groups and append pooled rows
#'
#' For each group defined by `group_vars`, fits a random-effects
#' meta-analysis via [metafor::rma()] with `method = "REML"` and appends a
#' pooled row to the data. Each cohort row receives a `weight` column
#' giving its REML contribution (in percent) to the pooled estimate;
#' pooled rows have `weight = NA`.
#'
#' Heterogeneity statistics (`I2`, `Q`, `tau2`) are attached to the pooled
#' row when k >= 2.
#'
#' When exactly two cohorts are present in a group, `same_direction` (sign
#' agreement) and `replicated` (sign agreement AND |β/SE| > 1.96 in the
#' second cohort) are computed for the pooled row. These concepts do not
#' generalize to k >= 3, so they are set to `NA` in that case.
#'
#' Groups with k = 1 after upstream filtering are retained as-is, with no
#' pooled row appended (and a warning).
#'
#' @param data A tidy effects data frame containing at minimum `cohort`,
#'   `estimate`, `std.error`, and `n_obs` columns.
#' @param group_vars Character vector of grouping columns
#'   (e.g., `c("outcome", "term")`).
#' @param cohort_col Name of the cohort identifier column. Default `"cohort"`.
#' @param discovery_label,replication_label Labels identifying the discovery
#'   and replication cohorts, used only when computing `same_direction` and
#'   `replicated` for k = 2 groups. If `NULL` (default), the first and
#'   second cohort values (in appearance order) are used.
#' @param pooled_label Label used for the appended pooled row. Default
#'   `"Pooled"`.
#' @param cohort_levels Optional character vector defining factor levels
#'   for the `cohort` column in the returned data. If `NULL`, a factor is
#'   built from the unique values with `pooled_label` last.
#'
#' @return `data` with pooled rows appended and a `weight` column added.
#'   New columns on the pooled rows: `I2`, `Q`, `tau2`, `n_cohorts`,
#'   `same_direction`, `replicated`. The pooled row's `n_obs` is the sum
#'   across cohorts (total subjects, not effective sample size).
#'
#' @export
add_meta_pooled_results <- function(data,
                                    group_vars,
                                    cohort_col        = "cohort",
                                    discovery_label   = NULL,
                                    replication_label = NULL,
                                    pooled_label      = "Pooled",
                                    cohort_levels     = NULL) {

  validate_args(
    data         = data,
    group_vars   = is_nonempty_character(),
    cohort_col   = is_string(),
    pooled_label = is_string(),
    custom_checks = list(
      list(
        condition = all(c(cohort_col, "estimate", "std.error", "n_obs") %in%
                          names(data)),
        message   = paste0(
          "{.arg data} must contain {.field {cohort_col}}, ",
          "{.field estimate}, {.field std.error}, and {.field n_obs}"
        )
      ),
      list(
        condition = is.null(discovery_label) ||
          (is.character(discovery_label) && length(discovery_label) == 1L),
        message   = "{.arg discovery_label} must be NULL or a single string"
      ),
      list(
        condition = is.null(replication_label) ||
          (is.character(replication_label) && length(replication_label) == 1L),
        message   = "{.arg replication_label} must be NULL or a single string"
      )
    )
  )

  preserve_cols <- setdiff(names(data), c(cohort_col, .RECOMPUTED_COLS))

  group_results <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::group_map(function(.x, .y) {
      .build_pooled_group(
        .x, .y, cohort_col, preserve_cols,
        discovery_label, replication_label, pooled_label
      )
    })

  out <- dplyr::bind_rows(group_results)

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


# Columns that are recomputed for the pooled row; everything else is
# carried through from the first cohort row of the group.
.RECOMPUTED_COLS <- c(
  "estimate", "std.error", "conf.low", "conf.high",
  "n_obs", "statistic", "p.value", "sig", "weight",
  "I2", "Q", "tau2", "n_cohorts",
  "same_direction", "replicated"
)


#' Build the cohort + pooled rows for one group, including REML weights.
#' @keywords internal
#' @noRd
.build_pooled_group <- function(cohort_rows, group_keys, cohort_col,
                                preserve_cols,
                                discovery_label, replication_label,
                                pooled_label) {

  k <- nrow(cohort_rows)

  if (k < 2L) {
    cli::cli_warn(c(
      "Skipping pooling for group with k = {k}:",
      "i" = "{paste(names(group_keys), unlist(group_keys), sep = ' = ', collapse = ', ')}"
    ))
    # Return the cohort row(s) unchanged but with weight = NA for shape consistency.
    cohort_rows$weight <- NA_real_
    return(cohort_rows)
  }

  m <- metafor::rma(
    yi     = cohort_rows$estimate,
    sei    = cohort_rows$std.error,
    method = "REML"
  )

  # REML weights: each cohort's % contribution to the pooled estimate.
  cohort_rows$weight <- as.numeric(stats::weights(m))

  # Two-cohort replication-style indicators
  same_dir <- NA
  repl     <- NA
  if (k == 2L) {
    disc_lab <- discovery_label   %||% cohort_rows[[cohort_col]][1]
    repl_lab <- replication_label %||% cohort_rows[[cohort_col]][2]

    disc_est <- cohort_rows$estimate[cohort_rows[[cohort_col]] == disc_lab][1]
    repl_est <- cohort_rows$estimate[cohort_rows[[cohort_col]] == repl_lab][1]
    repl_se  <- cohort_rows$std.error[cohort_rows[[cohort_col]] == repl_lab][1]

    same_dir <- sign(disc_est) == sign(repl_est)
    repl     <- isTRUE(same_dir) && abs(repl_est / repl_se) > 1.96
  }

  pooled <- tibble::tibble(
    !!!group_keys,
    !!cohort_col   := pooled_label,
    estimate       = as.numeric(m$b),
    std.error      = m$se,
    conf.low       = m$ci.lb,
    conf.high      = m$ci.ub,
    n_obs          = sum(cohort_rows$n_obs, na.rm = TRUE),
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

  # Carry forward group-constant metadata from the first cohort row
  for (col in preserve_cols) {
    if (col %in% names(cohort_rows)) {
      pooled[[col]] <- cohort_rows[[col]][1]
    }
  }

  dplyr::bind_rows(cohort_rows, pooled)
}
