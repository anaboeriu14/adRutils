#' Impute missing data using Random Forest algorithm
#'
#' Imputes missing values in specified variables using the missForest
#' algorithm, a non-parametric imputation method based on Random Forest.
#'
#' @param dataf A data frame containing the data to be imputed
#' @param vars_to_impute Character vector of column names to impute
#' @param helper_vars Character vector of additional column names used to improve imputation
#'   but not returned in the imputed columns
#' @param parallel Logical. If TRUE, uses parallel processing with 'forests' method. Default is TRUE.
#'   Note: Requires setting up parallel backend first (see Examples)
#' @param verbose Logical. If TRUE, shows progress during imputation. Default is TRUE
#'
#' @return A list containing:
#'   \item{data_imputed}{Original data frame with imputed columns added (prefixed with "i_")}
#'   \item{error_mse}{Data frame with out-of-bag imputation error for each variable}
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing (do this before calling the function)
#' doParallel::registerDoParallel(cores = 3)
#'
#' testD <- data.frame(
#'   BMI = c(22.5, NA, 24.0, 25.5, NA, 23.0, 28.0, NA, 21.5, 26.0),
#'   height = c(1.75, 1.80, 1.70, NA, 1.85, 1.65, 1.90, 1.78, NA, 1.68),
#'   weight = c(70, 80, NA, 85, 90, 60, 100, 75, 55, NA)
#' )
#'
#' result <- rf_impute_data(testD, "BMI", c("weight", "height"))
#'
#' # Access imputed data
#' imputed_data <- result$data_imputed
#'
#' # Check imputation errors
#' errors <- result$error_mse
#' }
#' @export
rf_impute_data <- function(dataf, vars_to_impute, helper_vars,
                           parallel = TRUE, verbose = TRUE) {

  # Validate inputs
  .validate_imputation_params(dataf, vars_to_impute, helper_vars)

  # Prepare data for imputation
  all_vars <- c(vars_to_impute, helper_vars)
  imputation_data <- dataf %>%
    dplyr::select(dplyr::all_of(all_vars)) %>%
    as.data.frame()

  # Get complete cases for error estimation
  complete_data <- .get_complete_cases(imputation_data, verbose)

  # Run imputation
  imputation_result <- .run_missforest(
    imputation_data,
    complete_data,
    parallel,
    verbose
  )

  # Format results
  return(.format_imputation_results(dataf, imputation_result, helper_vars))
}

#' Validate rf_impute_data parameters
#' @keywords internal
.validate_imputation_params <- function(dataf, vars_to_impute, helper_vars) {
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
      )
    ),
    context = "rf_impute_data"
  )

  invisible(TRUE)
}

#' Get complete cases for error estimation
#' @keywords internal
.get_complete_cases <- function(imputation_data, verbose) {
  complete_data <- imputation_data[complete.cases(imputation_data), ]

  # Warn if very few complete cases
  if (nrow(complete_data) < 5 && verbose) {
    cli::cli_alert_warning("Only {nrow(complete_data)} complete case{?s} available for error estimation")
  }

  return(complete_data)
}

#' Run missForest imputation
#' @keywords internal
.run_missforest <- function(imputation_data, complete_data, parallel, verbose) {
  parallelize <- if (parallel) "forests" else "no"

  missForest::missForest(
    xmis = imputation_data,
    variablewise = TRUE,
    verbose = verbose,
    xtrue = if (nrow(complete_data) > 0) complete_data else NULL,
    parallelize = parallelize
  )
}

#' Format imputation results into return list
#' @keywords internal
.format_imputation_results <- function(dataf, imputation_result, helper_vars) {
  # Create error dataframe
  error_df <- tibble::tibble(
    variable = paste0("i_", names(imputation_result$OOBerror)),
    MSE = imputation_result$OOBerror
  )

  # Extract and rename imputed columns
  imputed_df <- imputation_result$ximp %>%
    dplyr::select(-dplyr::all_of(helper_vars)) %>%
    dplyr::rename_with(~ paste0("i_", .), .cols = dplyr::everything()) %>%
    tibble::as_tibble()

  # Combine with original data
  data_imputed <- dplyr::bind_cols(dataf, imputed_df)

  return(list(
    data_imputed = data_imputed,
    error_mse = error_df
  ))
}
