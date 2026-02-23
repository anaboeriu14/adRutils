#' Create Pairwise Group Comparison Table
#'
#' Compares groups pairwise using t-tests (numeric) and chi-squared/Fisher tests
#' (categorical). Returns a formatted table with adjusted p-values.
#'
#' @param data A data frame
#' @param group_var Character string. Grouping variable name
#' @param numeric_vars Character vector. Numeric variables to compare via t-tests
#' @param categorical_vars Character vector. Categorical variables to compare
#'   via chi-squared tests with Fisher's exact fallback (default: NULL)
#' @param p_adjust_method P-value adjustment method (default: "bonferroni").
#'   Options: "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"
#' @param p_format P-value display format (default: "auto"):
#'   "auto", "threshold", "exact", "scientific", or "raw" (no formatting)
#' @param p_digits Number of decimal places or significant figures (default: 3)
#'
#' @return A tibble with columns: variable, test_type, and one column per group pair
#'
#' @examples
#' \dontrun{
#' create_pairwise_table(
#'   data = my_data,
#'   group_var = "ancestry",
#'   numeric_vars = c("age", "bmi"),
#'   categorical_vars = c("sex", "diagnosis"),
#'   p_format = "threshold"
#' )
#' }
#'
#' @export
create_pairwise_table <- function(data,
                                  group_var,
                                  numeric_vars,
                                  categorical_vars = NULL,
                                  p_adjust_method = "bonferroni",
                                  p_format = c("auto", "threshold", "exact",
                                               "scientific", "raw"),
                                  p_digits = 3) {

  p_format <- match.arg(p_format)

  all_vars <- c(numeric_vars, categorical_vars)

  validate_params(
    data = data,
    columns = c(group_var, all_vars),
    numeric_columns = numeric_vars,
    method = p_adjust_method,
    valid_methods = c("bonferroni", "holm", "hochberg", "hommel",
                      "BH", "BY", "fdr", "none"),
    custom_checks = list(
      list(
        condition = is.character(numeric_vars) && length(numeric_vars) > 0,
        message = "{.arg numeric_vars} must be a non-empty character vector"
      ),
      list(
        condition = is.null(categorical_vars) || is.character(categorical_vars),
        message = "{.arg categorical_vars} must be NULL or a character vector"
      ),
      list(
        condition = is.numeric(p_digits) && length(p_digits) == 1 && p_digits > 0,
        message = "{.arg p_digits} must be a single positive integer"
      ),
      list(
        condition = length(unique(data[[group_var]])) >= 2,
        message = "Grouping variable {.val {group_var}} must have at least 2 unique levels"
      )
    ),
    context = "create_pairwise_table"
  )

  groups <- levels(factor(data[[group_var]]))
  pairs <- combn(groups, 2, simplify = FALSE)
  pair_names <- map_chr(pairs, ~ paste(.x[1], "vs", .x[2]))

  numeric_results <- .run_numeric_comparisons(data, numeric_vars, group_var,
                                              pair_names, p_adjust_method)

  categorical_results <- .run_categorical_comparisons(data, categorical_vars,
                                                      group_var, pairs,
                                                      pair_names, p_adjust_method)

  combined <- bind_rows(numeric_results, categorical_results)

  if (p_format != "raw") {
    combined <- .format_pvalue_columns(combined, pair_names, p_format, p_digits)
  }

  return(combined)
}

# --- Internal helpers --------------------------------------------------------

#' Run pairwise.t.test for numeric variables
#' @keywords internal
.run_numeric_comparisons <- function(data, numeric_vars, group_var,
                                     pair_names, p_adjust_method) {
  if (length(numeric_vars) == 0) return(tibble())

  map_dfr(numeric_vars, function(var) {
    result <- tryCatch({
      pt <- pairwise.t.test(
        data[[var]],
        data[[group_var]],
        p.adjust.method = p_adjust_method
      )
      .extract_pairwise_matrix(pt$p.value)
    }, error = function(e) {
      setNames(as.list(rep(NA_real_, length(pair_names))), pair_names)
    })

    tibble(variable = var, test_type = "t-test") %>%
      bind_cols(as_tibble(result))
  })
}

#' Extract p-values from pairwise.t.test matrix into named list
#' @keywords internal
.extract_pairwise_matrix <- function(pvalue_matrix) {
  row_names <- rownames(pvalue_matrix)
  col_names <- colnames(pvalue_matrix)
  pairwise_results <- list()

  for (i in seq_along(row_names)) {
    for (j in seq_along(col_names)) {
      if (!is.na(pvalue_matrix[i, j])) {
        comparison_name <- paste(col_names[j], "vs", row_names[i])
        pairwise_results[[comparison_name]] <- pvalue_matrix[i, j]
      }
    }
  }

  return(pairwise_results)
}

#' Run chi-squared/Fisher tests for categorical variables
#' @keywords internal
.run_categorical_comparisons <- function(data, categorical_vars, group_var,
                                         pairs, pair_names, p_adjust_method) {
  if (is.null(categorical_vars) || length(categorical_vars) == 0) return(tibble())

  map_dfr(categorical_vars, function(var) {
    pvals <- map_dbl(pairs, function(pair) {
      sub <- data[data[[group_var]] %in% pair, , drop = FALSE]
      sub[[group_var]] <- factor(sub[[group_var]], levels = pair)
      tbl <- table(sub[[group_var]], sub[[var]])

      tryCatch(
        chisq.test(tbl)$p.value,
        error = function(e) {
          tryCatch(
            fisher.test(tbl, simulate.p.value = TRUE)$p.value,
            error = function(e2) NA_real_
          )
        }
      )
    })

    if (p_adjust_method != "none") {
      pvals <- p.adjust(pvals, method = p_adjust_method)
    }

    tibble(variable = var, test_type = "chi-squared/fisher") %>%
      bind_cols(setNames(as.list(pvals), pair_names))
  })
}

#' Format p-value columns
#' @keywords internal
.format_pvalue_columns <- function(results, pair_names, format, digits) {
  for (col in pair_names) {
    results[[col]] <- .format_pvalues(results[[col]], format, digits)
  }
  return(results)
}

#' Format a vector of p-values
#' @keywords internal
.format_pvalues <- function(pvals, format, digits) {
  switch(format,
         auto = ifelse(is.na(pvals), NA_character_,
                       ifelse(pvals >= 0.001,
                              as.character(signif(pvals, digits)),
                              formatC(pvals, format = "e", digits = digits))),
         threshold = ifelse(is.na(pvals), NA_character_,
                            ifelse(pvals < 0.001,
                                   "< 0.001",
                                   as.character(round(pvals, digits)))),
         exact = ifelse(is.na(pvals), NA_character_,
                        as.character(round(pvals, digits))),
         scientific = ifelse(is.na(pvals), NA_character_,
                             formatC(pvals, format = "e", digits = digits))
  )
}
