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
#'
#' @return A named list where each element contains a formatted p-value string.
#'   P-values < 0.001 are displayed as "< 0.001", others are rounded to 3 decimal places.
#'   Returns NULL if no valid comparisons can be made.
#'
#' @details
#' The function uses \code{pairwise.t.test()} to perform all pairwise comparisons
#' between groups. Comparison names are formatted as "Group1 vs Group2".
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- extract_pairwise_pvalues(mtcars, "mpg", "cyl", "bonferroni")
#'
#' # With different adjustment method
#' results <- extract_pairwise_pvalues(iris, "Sepal.Length", "Species", "holm")
#' }
#'
#' @export
extract_pairwise_pvalues <- function(dataf, numeric_var, group_var, p_adjust) {

  # input validation
  validate_params(
    data = dataf,
    columns = c(numeric_var, group_var),
    numeric_columns = numeric_var,
    method = p_adjust,
    valid_methods = c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"),
    custom_checks = list(
      list(
        condition = length(unique(dataf[[group_var]])) >= 2,
        message = paste0("Grouping variable '", group_var, "' must have at least 2 unique levels for pairwise comparisons")
      ),
      list(
        condition = sum(!is.na(dataf[[numeric_var]])) > 0,
        message = paste0("Numeric variable '", numeric_var, "' contains no non-missing values")
      )
    ),
    context = "extract_pairwise_pvalues"
  )

  post_hoc_test <- pairwise.t.test(dataf[[numeric_var]], dataf[[group_var]],
                                   p.adjust.method = p_adjust)

  if(is.null(post_hoc_test[["p.value"]])) return(NULL)

  formatted_pvals <- ifelse(post_hoc_test[["p.value"]] < 0.001, "< 0.001",
                            as.character(round(post_hoc_test[["p.value"]], 3)))

  # Get all pairwise comparisons from the matrix
  row_names <- rownames(formatted_pvals)
  col_names <- colnames(formatted_pvals)

  pairwise_results <- list()

  # Extract all available comparisons from the lower triangle matrix
  for(i in seq_along(row_names)) {
    for(j in seq_along(col_names)) {
      #skip the NAs ie g1 vs g1 etc
      if(!is.na(formatted_pvals[i, j])) {
        comparison_name <- paste(col_names[j], "vs", row_names[i])
        pairwise_results[[comparison_name]] <- formatted_pvals[i, j]
      }
    }
  }

  return(pairwise_results)
}

#' Create Pairwise Comparison Table for Multiple Variables
#'
#' Performs pairwise t-tests for multiple numeric variables against a grouping variable
#' and returns results in a wide table format suitable for reporting.
#'
#' @param dataf A data frame containing the data
#' @param variables Character vector of numeric variable names to test
#' @param group_var Character string specifying the name of the grouping variable
#' @param p_adjust Character string specifying the p-value adjustment method.
#'   Default is "bonferroni". Options include "bonferroni", "holm", "hochberg",
#'   "hommel", "BH", "BY", "fdr", "none"
#'
#' @return A tibble with variables as rows and pairwise comparisons as columns.
#'   Each cell contains a formatted p-value. The first column contains variable names.
#'
#' @details
#' This function is a wrapper around \code{extract_pairwise_pvalues()} that processes
#' multiple variables at once and formats the results in a publication-ready table.
#' P-values < 0.001 are displayed as "< 0.001", others are rounded to 3 decimal places.
#'
#' @examples
#' \dontrun{
#' # Test multiple variables
#' variables <- c("mpg", "hp", "wt")
#' results <- create_pairwise_table(mtcars, variables, "cyl")
#'
#' # With custom p-value adjustment
#' results <- create_pairwise_table(iris,
#'                                  c("Sepal.Length", "Sepal.Width"),
#'                                  "Species",
#'                                  p_adjust = "holm")
#' }
#'
#' @export

create_pairwise_table <- function(dataf, variables, group_var, p_adjust = "bonferroni") {

  # Validation
  valid_methods <- c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none")

  custom_checks <- list(
    list(
      condition = length(variables) > 0,
      message = "At least one variable must be specified in 'variables'"
    ),
    list(
      condition = length(unique(dataf[[group_var]])) >= 2,
      message = paste0("Grouping variable '", group_var, "' must have at least 2 unique values for pairwise comparisons")
    )
  )

  validate_params(
    data = dataf,
    columns = c(variables, group_var),
    numeric_columns = variables,
    method = p_adjust,
    valid_methods = valid_methods,
    custom_checks = custom_checks,
    context = "create_pairwise_table"
  )

  pvalue_results <- purrr::map(variables, ~extract_pairwise_pvalues(dataf, .x, group_var, p_adjust))
  names(pvalue_results) <- variables

  # Get all unique comparison names from the first non-null result
  comparison_names <- names(pvalue_results[[which(!sapply(pvalue_results, is.null))[1]]])

  # Create dynamic tibble with all comparisons
  purrr::map_dfr(pvalue_results, ~{
    if(is.null(.x)) {
      # Handle cases where post-hoc test returns NULL
      result <- as.list(rep(NA, length(comparison_names)))
      names(result) <- comparison_names
      tibble::as_tibble(result)
    } else {
      tibble::as_tibble(.x)
    }
  }, .id = "variable")
}
