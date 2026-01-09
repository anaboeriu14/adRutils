#' Impute missing data using Random Forest algorithm
#'
#' Imputes missing values in specified variables using the missForest
#' algorithm, a non-parametric imputation method based on Random Forest.
#'
#' @param dataf A data frame containing the data to be imputed
#' @param vars_to_impute Character vector of column names to impute
#' @param helper_vars Character vector of additional column names used to improve imputation
#'   but not returned in the imputed columns
#' @param parallel Logical. If TRUE, uses parallel processing with 'forests' method (default: TRUE).
#'   Note: Requires setting up parallel backend first (see Examples)
#' @param verbose Logical. If TRUE, shows progress during imputation (default: FALSE)
#'
#' @return A list containing:
#'   \item{data_imputed}{Original data frame with imputed columns added (prefixed with "i_")}
#'   \item{error_mse}{Data frame with out-of-bag imputation error for each variable}
#'
#' @details
#' This function uses the missForest package, which iteratively imputes missing values
#' using Random Forest models. The algorithm is particularly useful for:
#' \itemize{
#'   \item Mixed data types (numeric and categorical)
#'   \item Non-linear relationships
#'   \item Complex interactions between variables
#' }
#'
#' The function adds imputed columns with "i_" prefix, preserving original columns
#' for comparison.
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing (recommended for large datasets)
#' doParallel::registerDoParallel(cores = 3)
#'
#' # Simple imputation
#' result <- rf_impute_data(
#'   dataf = mydata,
#'   vars_to_impute = "BMI",
#'   helper_vars = c("weight", "height")
#' )
#'
#' # Access imputed data
#' imputed_data <- result$data_imputed
#'
#' # Check imputation errors
#' errors <- result$error_mse
#'
#' # Multiple variables with verbose output
#' result <- rf_impute_data(
#'   dataf = mydata,
#'   vars_to_impute = c("BMI", "glucose", "cholesterol"),
#'   helper_vars = c("age", "sex", "weight", "height"),
#'   parallel = TRUE,
#'   verbose = TRUE
#' )
#'
#' # Compare original vs imputed
#' summary(mydata$BMI) # Original with NAs
#' summary(result$data_imputed$i_BMI) # Imputed values
#' }
#'
#' @export
rf_impute_data <- function(dataf, vars_to_impute, helper_vars,
                           parallel = TRUE, verbose = FALSE) {
  # Validate inputs
  .validate_imputation_params(
    dataf, vars_to_impute, helper_vars,
    parallel, verbose
  )

  # Prepare data for imputation
  imputation_data <- .prepare_imputation_data(dataf, vars_to_impute, helper_vars)

  # Get complete cases for error estimation
  complete_data <- .get_complete_cases(imputation_data, verbose)

  # Run imputation
  imputation_result <- .run_missforest(
    imputation_data,
    complete_data,
    parallel,
    verbose
  )

  # Format and return results
  .format_imputation_results(dataf, imputation_result, vars_to_impute, helper_vars)
}

#' Validate rf_impute_data parameters
#' @keywords internal
#' @noRd
.validate_imputation_params <- function(dataf, vars_to_impute, helper_vars,
                                        parallel, verbose) {
  all_vars <- c(vars_to_impute, helper_vars)

  validate_params(
    data = dataf,
    columns = all_vars,
    custom_checks = list(
      list(
        condition = is.character(vars_to_impute) && length(vars_to_impute) > 0,
        message = "{.arg vars_to_impute} must be a non-empty character vector"
      ),
      list(
        condition = is.character(helper_vars) && length(helper_vars) > 0,
        message = "{.arg helper_vars} must be a non-empty character vector"
      ),
      list(
        condition = is.logical(parallel) && length(parallel) == 1,
        message = "{.arg parallel} must be a single logical value"
      ),
      list(
        condition = is.logical(verbose) && length(verbose) == 1,
        message = "{.arg verbose} must be a single logical value"
      ),
      list(
        condition = length(intersect(vars_to_impute, helper_vars)) == 0,
        message = "{.arg vars_to_impute} and {.arg helper_vars} must not overlap"
      )
    ),
    context = "rf_impute_data"
  )

  invisible(TRUE)
}

#' Prepare data for imputation
#' @keywords internal
#' @noRd
.prepare_imputation_data <- function(dataf, vars_to_impute, helper_vars) {
  all_vars <- c(vars_to_impute, helper_vars)

  imputation_data <- dataf %>%
    dplyr::select(dplyr::all_of(all_vars)) %>%
    as.data.frame()

  return(imputation_data)
}

#' Get complete cases for error estimation
#' @keywords internal
#' @noRd
.get_complete_cases <- function(imputation_data, verbose) {
  complete_data <- imputation_data[complete.cases(imputation_data), ]

  # Warn if very few complete cases
  if (nrow(complete_data) < 5 && verbose) {
    cli::cli_alert_warning(
      "Only {nrow(complete_data)} complete case{?s} available for error estimation"
    )
  }

  if (nrow(complete_data) == 0 && verbose) {
    cli::cli_alert_info("No complete cases available - error estimation will be skipped")
  }

  return(complete_data)
}

#' Run missForest imputation
#' @keywords internal
#' @noRd
.run_missforest <- function(imputation_data, complete_data, parallel, verbose) {
  parallelize <- if (parallel) "forests" else "no"

  # Determine xtrue parameter
  xtrue_data <- if (nrow(complete_data) > 0) complete_data else NULL

  if (verbose) {
    # Let missForest show its messages
    result <- missForest::missForest(
      xmis = imputation_data,
      variablewise = TRUE,
      verbose = TRUE,
      xtrue = xtrue_data,
      parallelize = parallelize
    )
  } else {
    # Suppress missForest's verbose output
    result <- suppressMessages(
      missForest::missForest(
        xmis = imputation_data,
        variablewise = TRUE,
        verbose = FALSE,
        xtrue = xtrue_data,
        parallelize = parallelize
      )
    )
  }

  return(result)
}

#' Format imputation results into return list
#' @keywords internal
#' @noRd
.format_imputation_results <- function(dataf, imputation_result,
                                       vars_to_impute, helper_vars) {
  # Create error dataframe
  error_df <- tibble::tibble(
    variable = paste0("i_", names(imputation_result$OOBerror)),
    MSE = imputation_result$OOBerror
  )

  # Extract imputed columns (only vars_to_impute, not helper_vars)
  imputed_df <- imputation_result$ximp %>%
    dplyr::select(dplyr::all_of(vars_to_impute)) %>%
    dplyr::rename_with(~ paste0("i_", .), .cols = dplyr::everything()) %>%
    tibble::as_tibble()

  # Combine with original data
  data_imputed <- dplyr::bind_cols(dataf, imputed_df)

  return(list(
    data_imputed = data_imputed,
    error_mse = error_df
  ))
}
