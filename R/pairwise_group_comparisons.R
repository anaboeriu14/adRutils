#' Create a pairwise group comparison table
#'
#' Compares groups pairwise using t-tests for numeric variables and
#' chi-squared tests (with Fisher's exact fallback) for categorical
#' variables. Returns a tidy table with one row per variable and one
#' column per group pair.
#'
#' @details
#' Numeric variables use [stats::pairwise.t.test()] with the supplied
#' `p_adjust_method`. Categorical variables compute pairwise tests
#' independently and then adjust via [stats::p.adjust()] with the same
#' method.
#'
#' Pair column names are human-readable (e.g., `"AFR vs EUR"`), so
#' programmatic access requires backticks: `` result$`AFR vs EUR` ``.
#'
#' @param data A data frame.
#' @param group_var Name of the grouping variable.
#' @param numeric_vars Character vector of numeric variables to compare
#'   via t-tests.
#' @param categorical_vars Character vector of categorical variables.
#'   Default `NULL`.
#' @param p_adjust_method P-value adjustment method. Default `"bonferroni"`.
#'   See [stats::p.adjust()] for options.
#' @param p_format Display format for p-values. One of `"auto"`,
#'   `"threshold"`, `"exact"`, `"scientific"`, or `"raw"` (no formatting).
#'   Default `"auto"`.
#' @param p_digits Decimal places or significant figures (depending on
#'   format). Default `3`.
#'
#' @return A tibble with `variable`, `test_type`, and one column per
#'   group pair.
#'
#' @examples
#' \dontrun{
#' create_pairwise_table(
#'   data             = my_data,
#'   group_var        = "ancestry",
#'   numeric_vars     = c("age", "bmi"),
#'   categorical_vars = c("sex", "diagnosis"),
#'   p_format         = "threshold"
#' )
#' }
#'
#' @export
create_pairwise_table <- function(data,
                                  group_var,
                                  numeric_vars,
                                  categorical_vars = NULL,
                                  p_adjust_method  = "bonferroni",
                                  p_format         = c("auto", "threshold",
                                                       "exact", "scientific",
                                                       "raw"),
                                  p_digits         = 3) {

  p_format <- match.arg(p_format)
  all_vars <- c(numeric_vars, categorical_vars)

  validate_args(
    data            = data,
    columns         = c(group_var, all_vars),
    numeric_columns = numeric_vars,
    group_var       = is_string(),
    numeric_vars    = is_nonempty_character(),
    p_adjust_method = is_one_of(c("bonferroni", "holm", "hochberg", "hommel",
                                  "BH", "BY", "fdr", "none")),
    p_digits        = is_number(positive = TRUE),
    custom_checks = list(
      list(
        condition = is.null(categorical_vars) || is.character(categorical_vars),
        message   = "{.arg categorical_vars} must be NULL or a character vector"
      ),
      list(
        condition = length(unique(data[[group_var]])) >= 2L,
        message   = "Grouping variable {.val {group_var}} must have >= 2 unique levels"
      )
    )
  )

  groups     <- levels(factor(data[[group_var]]))
  pairs      <- utils::combn(groups, 2, simplify = FALSE)
  pair_names <- purrr::map_chr(pairs, ~ paste(.x[1], "vs", .x[2]))

  numeric_results <- .run_numeric_comparisons(
    data, numeric_vars, group_var, pair_names, p_adjust_method
  )

  cat_result <- .run_categorical_comparisons(
    data, categorical_vars, group_var, pairs, pair_names, p_adjust_method
  )

  if (cat_result$n_fisher > 0L) {
    cli::cli_alert_info(
      "Used Fisher's exact for {cat_result$n_fisher} comparison{?s} (sparse table{?s})"
    )
  }

  combined <- dplyr::bind_rows(numeric_results, cat_result$table)

  if (p_format != "raw") {
    combined <- .format_pvalue_columns(combined, pair_names, p_format, p_digits)
  }

  combined
}


#' Run pairwise.t.test() for each numeric variable.
#' @keywords internal
#' @noRd
.run_numeric_comparisons <- function(data, numeric_vars, group_var,
                                     pair_names, p_adjust_method) {
  if (length(numeric_vars) == 0L) return(tibble::tibble())

  purrr::map_dfr(numeric_vars, function(var) {
    result <- tryCatch({
      pt <- stats::pairwise.t.test(
        data[[var]], data[[group_var]],
        p.adjust.method = p_adjust_method
      )
      .extract_pairwise_matrix(pt$p.value)
    }, error = function(e) {
      stats::setNames(as.list(rep(NA_real_, length(pair_names))), pair_names)
    })

    tibble::tibble(variable = var, test_type = "t-test") %>%
      dplyr::bind_cols(tibble::as_tibble(result))
  })
}


#' Convert pairwise.t.test()'s lower-triangular matrix into a named list.
#' @keywords internal
#' @noRd
.extract_pairwise_matrix <- function(pvalue_matrix) {
  rows <- rownames(pvalue_matrix)
  cols <- colnames(pvalue_matrix)
  out  <- list()

  for (i in seq_along(rows)) {
    for (j in seq_along(cols)) {
      if (!is.na(pvalue_matrix[i, j])) {
        out[[paste(cols[j], "vs", rows[i])]] <- pvalue_matrix[i, j]
      }
    }
  }
  out
}


#' Run chi-squared / Fisher tests per pair, per variable. Returns the
#' result tibble plus a count of Fisher-fallback uses.
#' @keywords internal
#' @noRd
.run_categorical_comparisons <- function(data, categorical_vars, group_var,
                                         pairs, pair_names, p_adjust_method) {
  if (is.null(categorical_vars) || length(categorical_vars) == 0L) {
    return(list(table = tibble::tibble(), n_fisher = 0L))
  }

  n_fisher <- 0L

  table <- purrr::map_dfr(categorical_vars, function(var) {
    pvals <- purrr::map_dbl(pairs, function(pair) {
      in_pair <- data[[group_var]] %in% pair
      tbl <- table(
        factor(data[[group_var]][in_pair], levels = pair),
        data[[var]][in_pair]
      )

      tryCatch(
        stats::chisq.test(tbl)$p.value,
        error = function(e) {
          n_fisher <<- n_fisher + 1L
          tryCatch(
            stats::fisher.test(tbl, simulate.p.value = TRUE)$p.value,
            error = function(e2) NA_real_
          )
        }
      )
    })

    if (p_adjust_method != "none") {
      pvals <- stats::p.adjust(pvals, method = p_adjust_method)
    }

    tibble::tibble(variable = var, test_type = "chi-squared/fisher") %>%
      dplyr::bind_cols(stats::setNames(as.list(pvals), pair_names))
  })

  list(table = table, n_fisher = n_fisher)
}


#' Apply p-value formatting to every pair column.
#' @keywords internal
#' @noRd
.format_pvalue_columns <- function(results, pair_names, format, digits) {
  for (col in pair_names) {
    results[[col]] <- .format_pvalues(results[[col]], format, digits)
  }
  results
}


#' Format a vector of p-values according to `format`.
#' @keywords internal
#' @noRd
.format_pvalues <- function(pvals, format, digits) {
  out <- rep(NA_character_, length(pvals))
  ok  <- !is.na(pvals)

  out[ok] <- switch(format,
                    auto       = ifelse(pvals[ok] >= 0.001,
                                        as.character(signif(pvals[ok], digits)),
                                        formatC(pvals[ok], format = "e", digits = digits)),
                    threshold  = ifelse(pvals[ok] < 0.001, "< 0.001",
                                        as.character(round(pvals[ok], digits))),
                    exact      = as.character(round(pvals[ok], digits)),
                    scientific = formatC(pvals[ok], format = "e", digits = digits)
  )
  out
}
