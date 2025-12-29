#' Extract and Format Pairwise T-Test P-values for One Variable
#'
#' Performs pairwise t-tests between groups and returns formatted p-values
#' for all possible group comparisons.
#'
#' @param dataf A data frame containing the data
#' @param numeric_var Character string specifying the name of the numeric variable to test
#' @param group_var Character string specifying the name of the grouping variable
#' @param p_adjust Character string specifying the p-value adjustment method.
#'   Options include "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"
#' @param p_format Character string specifying p-value display format (default: "auto"):
#'   \describe{
#'     \item{auto}{Smart format: decimals for p >= 0.001, scientific for p < 0.001}
#'     \item{threshold}{Shows "< 0.001" for very small p-values, decimals otherwise}
#'     \item{exact}{Always shows decimals (may display "0.000" for very small p-values)}
#'     \item{scientific}{Always shows scientific notation (e.g., 4.50e-02)}
#'   }
#' @param p_digits Integer specifying number of decimal places.
#'   For scientific format, specifies significant figures
#'
#' @return A named list where each element contains a formatted p-value string.
#'   Returns NULL if no valid comparisons can be made.
#'
#' @details
#' The function uses \code{pairwise.t.test()} to perform all pairwise comparisons
#' between groups. Comparison names are formatted as "Group1 vs Group2".
#'
#' @seealso \code{\link{create_pairwise_table}} for multiple variables at once
#'
#' @examples
#' \dontrun{
#' # Auto format (smart default)
#' results <- extract_pairwise_pvalues(mtcars, "mpg", "cyl", "bonferroni")
#' # Returns: "0.045" for readable values, "3.00e-04" for tiny values
#'
#' # Threshold format (publication style)
#' results <- extract_pairwise_pvalues(mtcars, "mpg", "cyl", "bonferroni",
#'                                     p_format = "threshold")
#' # Returns: "< 0.001", "0.023", "0.156"
#'
#' # Scientific notation (exact magnitude)
#' results <- extract_pairwise_pvalues(iris, "Sepal.Length", "Species", "holm",
#'                                     p_format = "scientific", p_digits = 2)
#' # Returns: "4.5e-02", "3.0e-04", "1.6e-01"
#' }
#'
#' @export
extract_pairwise_pvalues <- function(dataf, numeric_var, group_var, p_adjust, p_digits,
                                     p_format = c("auto", "threshold", "exact", "scientific")
                                     ) {

  # Validate and match p_format
  p_format <- match.arg(p_format)

  # Validate inputs
  validate_params(
    data = dataf,
    columns = c(numeric_var, group_var),
    numeric_columns = numeric_var,
    method = p_adjust,
    valid_methods = c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"),
    custom_checks = list(
      list(
        condition = is.numeric(p_digits) && length(p_digits) == 1 && p_digits > 0,
        message = "{.arg p_digits} must be a single positive integer"
      ),
      list(
        condition = length(unique(dataf[[group_var]])) >= 2,
        message = "Grouping variable '{group_var}' must have at least 2 unique levels for pairwise comparisons"
      ),
      list(
        condition = sum(!is.na(dataf[[numeric_var]])) > 0,
        message = "Numeric variable '{numeric_var}' contains no non-missing values"
      )
    ),
    context = "extract_pairwise_pvalues"
  )

  # Perform pairwise t-tests
  post_hoc_test <- pairwise.t.test(
    dataf[[numeric_var]],
    dataf[[group_var]],
    p.adjust.method = p_adjust
  )

  # Check if test produced results
  if (is.null(post_hoc_test[["p.value"]])) {
    return(NULL)
  }

  # Format and return p-values
  return(.format_pairwise_results(post_hoc_test[["p.value"]],
                                  format = p_format,
                                  digits = p_digits))
}

#' Create Pairwise Comparison Table for Multiple Variables
#'
#' Performs pairwise t-tests for multiple numeric variables against a grouping variable
#' and returns results in a wide table format suitable for reporting.
#'
#' @param dataf A data frame containing the data
#' @param variables Character vector of numeric variable names to test
#' @param group_var Character string specifying the name of the grouping variable
#' @param p_adjust Character string specifying the p-value adjustment method
#'   (default: "bonferroni"). Options include "bonferroni", "holm", "hochberg",
#'   "hommel", "BH", "BY", "fdr", "none"
#' @param p_format Character string specifying p-value display format (default: "auto").
#'   See \code{\link{extract_pairwise_pvalues}} for format options
#' @param p_digits Integer specifying number of decimal places
#'
#' @return A tibble with variables as rows and pairwise comparisons as columns.
#'   Each cell contains a formatted p-value. The first column contains variable names.
#'
#' @details
#' This function is a wrapper around \code{\link{extract_pairwise_pvalues}} that processes
#' multiple variables at once and formats the results in a publication-ready table.
#'
#' @seealso \code{\link{extract_pairwise_pvalues}} for analyzing a single variable
#'
#' @examples
#' \dontrun{
#' # Auto format (default - smart)
#' results <- create_pairwise_table(mtcars, c("mpg", "hp", "wt"), "cyl")
#'
#' # Threshold format (publication style)
#' results <- create_pairwise_table(mtcars, c("mpg", "hp"), "cyl",
#'                                  p_format = "threshold")
#'
#' # With custom adjustment and formatting
#' results <- create_pairwise_table(iris,
#'                                  c("Sepal.Length", "Sepal.Width"),
#'                                  "Species",
#'                                  p_adjust = "holm",
#'                                  p_format = "scientific",
#'                                  p_digits = 2)
#' }
#'
#' @export
create_pairwise_table <- function(dataf, variables, group_var, p_digits,
                                  p_adjust = "bonferroni",
                                  p_format = c("auto", "threshold", "exact", "scientific")
                                  ) {

  # Validate and match p_format
  p_format <- match.arg(p_format)

  # Validate inputs
  validate_params(
    data = dataf,
    columns = c(variables, group_var),
    numeric_columns = variables,
    method = p_adjust,
    valid_methods = c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"),
    custom_checks = list(
      list(
        condition = is.numeric(p_digits) && length(p_digits) == 1 && p_digits > 0,
        message = "{.arg p_digits} must be a single positive integer"
      ),
      list(
        condition = length(variables) > 0,
        message = "At least one variable must be specified in {.arg variables}"
      ),
      list(
        condition = length(unique(dataf[[group_var]])) >= 2,
        message = "Grouping variable '{group_var}' must have at least 2 unique values for pairwise comparisons"
      )
    ),
    context = "create_pairwise_table"
  )

  # Extract p-values for each variable with specified formatting
  pvalue_results <- map(
    variables,
    ~extract_pairwise_pvalues(dataf, .x, group_var, p_adjust,
                              p_format = p_format, p_digits = p_digits)
  )
  names(pvalue_results) <- variables

  # Get comparison names from first non-null result
  first_valid <- which(!sapply(pvalue_results, is.null))[1]

  if (is.na(first_valid)) {
    cli::cli_alert_warning("No valid pairwise comparisons could be computed")
    return(tibble(variable = variables))
  }

  comparison_names <- names(pvalue_results[[first_valid]])

  # Build result table
  result_table <- map_dfr(pvalue_results, ~{
    if (is.null(.x)) {
      # Handle NULL results (no valid comparisons)
      result <- as.list(rep(NA_character_, length(comparison_names)))
      names(result) <- comparison_names
      tibble::as_tibble(result)
    } else {
      tibble::as_tibble(.x)
    }
  }, .id = "variable")

  return(result_table)
}

#' Format pairwise test results with flexible formatting options
#' @keywords internal
#' Format pairwise test results with flexible formatting options
#' @keywords internal
.format_pairwise_results <- function(pvalue_matrix,
                                     format = c("auto", "threshold", "exact", "scientific"),
                                     digits = 3) {

  format <- match.arg(format)

  # Format p-values based on selected format
  formatted_pvals <- switch(format,
                            auto = {
                              # Use signif for better precision across magnitudes
                              ifelse(pvalue_matrix >= 0.001,
                                     as.character(signif(pvalue_matrix, digits)),
                                     formatC(pvalue_matrix, format = "e", digits = digits))
                            },
                            threshold = {
                              # Use round for consistent decimal alignment
                              ifelse(pvalue_matrix < 0.001,
                                     "< 0.001",
                                     as.character(round(pvalue_matrix, digits)))
                            },
                            exact = {
                              # Use round for table alignment
                              as.character(round(pvalue_matrix, digits))
                            },
                            scientific = {
                              # Scientific notation
                              formatC(pvalue_matrix, format = "e", digits = digits)
                            }
  )

  # Extract row and column names
  row_names <- rownames(formatted_pvals)
  col_names <- colnames(formatted_pvals)

  pairwise_results <- list()

  # Build named list of comparisons
  for (i in seq_along(row_names)) {
    for (j in seq_along(col_names)) {
      if (!is.na(formatted_pvals[i, j])) {
        comparison_name <- paste(col_names[j], "vs", row_names[i])
        pairwise_results[[comparison_name]] <- formatted_pvals[i, j]
      }
    }
  }

  return(pairwise_results)
}
