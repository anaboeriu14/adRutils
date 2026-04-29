#' Impute missing data using random forest
#'
#' Imputes missing values in `vars_to_impute` using
#' [missForest::missForest()], a non-parametric Random Forest imputation
#' method. Helper variables contribute to the imputation model but are
#' not themselves imputed in the output.
#'
#' Imputed columns are added with an `i_` prefix; original columns are
#' preserved unchanged.
#'
#' @param dataf A data frame.
#' @param vars_to_impute Character vector of columns to impute.
#' @param helper_vars Character vector of additional columns used as
#'   predictors during imputation. Must not overlap `vars_to_impute`.
#' @param parallel If `TRUE`, use the `"forests"` parallelization mode of
#'   [missForest::missForest()]. Default `FALSE`. Requires registering a
#'   parallel backend (e.g., [doParallel::registerDoParallel()]) *before*
#'   calling.
#' @param quiet If `TRUE`, suppress messages. Default `FALSE`.
#'
#' @return `dataf` with imputed columns appended (named `i_<var>`). The
#'   per-variable out-of-bag imputation error is attached as an attribute
#'   named `"imputation_errors"` (a tibble with `variable` and `MSE`
#'   columns); access it via `attr(result, "imputation_errors")`.
#'
#' @details
#' For error estimation, [missForest::missForest()] is given the complete
#' cases of the input as `xtrue`. When few complete cases are available,
#' the resulting error estimates may be unreliable; a warning is issued
#' if fewer than 10% of rows are complete.
#'
#' @examples
#' \dontrun{
#' # Optional: register a parallel backend before calling with parallel = TRUE
#' doParallel::registerDoParallel(cores = 3)
#'
#' result <- impute_random_forest(
#'   dataf          = mydata,
#'   vars_to_impute = c("BMI", "glucose"),
#'   helper_vars    = c("age", "sex", "weight"),
#'   parallel       = TRUE
#' )
#'
#' summary(result$i_BMI)
#' attr(result, "imputation_errors")
#' }
#'
#' @export
impute_random_forest <- function(dataf,
                                 vars_to_impute,
                                 helper_vars,
                                 parallel = FALSE,
                                 quiet    = FALSE) {

  validate_args(
    data           = dataf,
    columns        = c(vars_to_impute, helper_vars),
    vars_to_impute = is_nonempty_character(),
    helper_vars    = is_nonempty_character(),
    parallel       = is_flag(),
    quiet          = is_flag(),
    custom_checks = list(
      list(
        condition = length(intersect(vars_to_impute, helper_vars)) == 0L,
        message   = "{.arg vars_to_impute} and {.arg helper_vars} must not overlap"
      )
    )
  )

  if (!requireNamespace("missForest", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg missForest} package is required for random-forest imputation",
      "i" = "Install with: {.code install.packages(\"missForest\")}"
    ))
  }

  imputation_data <- dataf %>%
    dplyr::select(dplyr::all_of(c(vars_to_impute, helper_vars))) %>%
    as.data.frame()

  complete_data <- imputation_data[stats::complete.cases(imputation_data), ]
  pct_complete  <- nrow(complete_data) / nrow(imputation_data)

  if (!quiet && pct_complete < 0.10) {
    cli::cli_alert_warning(
      "Only {nrow(complete_data)} complete case{?s} ({round(pct_complete * 100, 1)}%); error estimates may be unreliable"
    )
  }

  fit <- .run_missforest(imputation_data, complete_data, parallel, quiet)

  imputed_df <- fit$ximp %>%
    dplyr::select(dplyr::all_of(vars_to_impute)) %>%
    dplyr::rename_with(~ paste0("i_", .x), .cols = dplyr::everything()) %>%
    tibble::as_tibble()

  result <- dplyr::bind_cols(dataf, imputed_df)

  attr(result, "imputation_errors") <- tibble::tibble(
    variable = paste0("i_", names(fit$OOBerror)),
    MSE      = fit$OOBerror
  )

  result
}


#' Wrap missForest with verbosity and parallel handling.
#' @keywords internal
#' @noRd
.run_missforest <- function(imputation_data, complete_data, parallel, quiet) {
  parallelize <- if (parallel) "forests" else "no"
  xtrue_data  <- if (nrow(complete_data) > 0L) complete_data else NULL

  args <- list(
    xmis         = imputation_data,
    variablewise = TRUE,
    verbose      = !quiet,
    xtrue        = xtrue_data,
    parallelize  = parallelize
  )

  if (quiet) {
    suppressMessages(do.call(missForest::missForest, args))
  } else {
    do.call(missForest::missForest, args)
  }
}
