#' Impute missing data using Random Forest algorithm
#'
#' This function imputes missing values in specified variables using the missForest
#' algorithm, which is a non-parametric imputation method based on Random Forest.
#'
#' @param dataf A data frame containing the data to be imputed.
#' @param vars_to_impute A character vector of column names to impute.
#' @param helper_vars A character vector of additional column names used to improve imputation but not returned in the imputed columns.
#' @param parallel Logical or character. If TRUE, uses parallel processing with 'forests' method. Default is TRUE.
#'                 Note: Requires setting up parallel backend first (see Examples).
#' @param verbose Logical. If TRUE, shows progress during imputation. Default is TRUE.
#'
#' @return A list containing:
#'   \item{newD_imputed}{Original data frame with imputed columns added (prefixed with "i_")}
#'   \item{error_mse}{Data frame with out-of-bag imputation error for each variable}
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing (do this before calling the function)
#' doParallel::registerDoParallel(cores = 3)
#' testD <- data.frame(BMI = c(22.5, NA, 24.0, 25.5, NA, 23.0, 28.0, NA, 21.5, 26.0),
#' height = c(1.75, 1.80, 1.70, NA, 1.85, 1.65, 1.90, 1.78, NA, 1.68),
#' weight = c(70, 80, NA, 85, 90, 60, 100, 75, 55, NA)
#' )
#' res <-  rf_impute_data(testD, "BMI", c("weight", "height"))
#' # Access imputed data
#' imputed_data <- result$newD_imputed
#'
#' # Check imputation errors
#' errors <- result$error_mse
#'}
#' @export
rf_impute_data <- function(dataf, vars_to_impute, helper_vars,
                           parallel = TRUE, verbose = TRUE) {

  # Input validation
  if (!is.data.frame(dataf)) {
    stop("'dataf' must be a data frame")
  }

  if (!is.character(vars_to_impute) || length(vars_to_impute) == 0) {
    stop("'vars_to_impute' must be a non-empty character vector")
  }

  if (!is.character(helper_vars)) {
    stop("'helper_vars' must be a character vector")
  }

  # Check for missing columns
  all_vars <- c(vars_to_impute, helper_vars)
  missing_vars <- base::setdiff(all_vars, colnames(dataf))

  if (length(missing_vars) > 0) {
    stop("Missing required columns: ", paste(missing_vars, collapse = ", "))
  }

  # Setup parallelization parameter
  parallelize <- if (parallel) 'forests' else 'no'

  # Prepare data for imputation
  imputation_data <- dataf %>%
    dplyr::select(dplyr::all_of(all_vars)) %>%
    as.data.frame()

  # Get reference data for error calculation (complete cases only)
  complete_data <- imputation_data[complete.cases(imputation_data), ]

  # Check if there's enough complete data for reference
  if (nrow(complete_data) < 5 && verbose) {
    warning("Very few complete cases available for error estimation")
  }

  # Run imputation
  imputation_result <- missForest::missForest(
    xmis = imputation_data,
    variablewise = TRUE,
    verbose = verbose,
    xtrue = if (nrow(complete_data) > 0) complete_data else NULL,
    parallelize = parallelize
  )

  # Create error dataframe
  error_df <- data.frame(
    variable = paste0("i_", names(imputation_result$OOBerror)),
    MSE = imputation_result$OOBerror,
    stringsAsFactors = FALSE
  )

  # Extract and rename imputed columns
  imputed_df <- imputation_result$ximp %>%
    dplyr::select(-dplyr::all_of(helper_vars)) %>%
    dplyr::rename_with(~ paste0("i_", .), .cols = dplyr::everything()) %>%
    tibble::as_tibble()

  # Combine with original data
  full_data_imputed <- dplyr::bind_cols(dataf, imputed_df)

  # Return results
  return(list(
    newD_imputed = full_data_imputed,
    error_mse = error_df
  ))
}
