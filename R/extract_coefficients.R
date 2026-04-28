#' Extract raw coefficients with confidence intervals
#'
#' Extracts coefficients from a tibble of fitted models (typically the output
#' of [fit_models_by_group()]) and adds confidence intervals computed from
#' the model standard errors.
#'
#' @param model_results A tibble containing a list-column `model_res` of tidy
#'   model results (as produced by [fit_models_by_group()]).
#' @param term_pattern Optional regex to filter `term` values.
#' @param ci_level Confidence level for the intervals. Default `0.95`.
#'
#' @return A tibble with one row per coefficient, including columns
#'   `estimate`, `std.error`, `conf.low`, `conf.high`, `p.value`, and `sig`
#'   (TRUE when the CI excludes zero).
#'
#' @examples
#' \dontrun{
#' results <- fit_models_by_group(data, outcomes, predictors)
#' extract_coefficients(results, term_pattern = "treatment")
#' }
#'
#' @export
extract_coefficients <- function(model_results,
                                 term_pattern = NULL,
                                 ci_level     = 0.95) {

  validate_args(
    data     = model_results,
    ci_level = is_number(min = 0, max = 1),
    custom_checks = list(
      list(
        condition = "model_res" %in% names(model_results),
        message   = paste0(
          "{.arg model_results} must contain a {.field model_res} list-column ",
          "(produced by {.fn fit_models_by_group})"
        )
      ),
      list(
        condition = is.null(term_pattern) ||
          (is.character(term_pattern) && length(term_pattern) == 1L),
        message   = "{.arg term_pattern} must be NULL or a single regex string"
      )
    )
  )

  z_val <- stats::qnorm((1 + ci_level) / 2)

  result <- model_results %>%
    dplyr::select(-dplyr::any_of(c("model_obj", "dataf"))) %>%
    tidyr::unnest(cols = dplyr::all_of("model_res"))

  if (!is.null(term_pattern)) {
    result <- dplyr::filter(result, stringr::str_detect(.data$term, term_pattern))
  }

  result %>%
    dplyr::mutate(
      conf.low  = .data$estimate - .data$std.error * z_val,
      conf.high = .data$estimate + .data$std.error * z_val,
      sig       = sign(.data$conf.low) == sign(.data$conf.high)
    )
}


#' Extract standardized coefficients
#'
#' Extracts standardized coefficients from a tibble of fitted models using
#' [parameters::standardize_parameters()].
#'
#' @param model_results A tibble containing a list-column `model_obj` of
#'   fitted model objects (as produced by [fit_models_by_group()]).
#' @param term_pattern Optional regex to filter `Parameter` values.
#' @param method Standardization method passed to
#'   [parameters::standardize_parameters()]. Default `"posthoc"`.
#'
#' @return A tibble with standardized coefficients and `sig` indicating
#'   whether the standardized CI excludes zero.
#'
#' @examples
#' \dontrun{
#' results <- fit_models_by_group(data, outcomes, predictors)
#' extract_standardized_coefficients(results, term_pattern = "age")
#' }
#'
#' @export
extract_standardized_coefficients <- function(model_results,
                                              term_pattern = NULL,
                                              method       = "posthoc") {

  validate_args(
    data   = model_results,
    method = is_string(),
    custom_checks = list(
      list(
        condition = "model_obj" %in% names(model_results),
        message   = paste0(
          "{.arg model_results} must contain a {.field model_obj} list-column ",
          "(produced by {.fn fit_models_by_group})"
        )
      ),
      list(
        condition = is.null(term_pattern) ||
          (is.character(term_pattern) && length(term_pattern) == 1L),
        message   = "{.arg term_pattern} must be NULL or a single regex string"
      )
    )
  )

  if (!requireNamespace("parameters", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg parameters} package required for standardized coefficients",
      "i" = "Install with: {.code install.packages('parameters')}"
    ))
  }

  result <- model_results %>%
    dplyr::mutate(
      std_params = purrr::map(.data$model_obj, function(m) {
        if (is.null(m)) return(NULL)
        parameters::standardize_parameters(m, method = method)
      })
    ) %>%
    dplyr::select(-dplyr::any_of(c("model_obj", "model_res", "dataf"))) %>%
    tidyr::unnest(cols = dplyr::all_of("std_params"))

  if (!is.null(term_pattern)) {
    result <- dplyr::filter(
      result,
      stringr::str_detect(.data$Parameter, term_pattern)
    )
  }

  result %>%
    dplyr::mutate(sig = sign(.data$CI_low) == sign(.data$CI_high))
}
