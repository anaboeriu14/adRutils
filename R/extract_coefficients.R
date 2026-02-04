#' Extract Raw Coefficients
#'
#' Extracts and formats coefficients from model results with confidence intervals.
#'
#' @param model_results Tibble from fit_models_by_group() containing model_res column
#' @param term_pattern Regex pattern to filter terms (optional)
#' @param ci_level Confidence level (default: 0.95)
#'
#' @return Tibble with coefficients, CIs, and significance flag
#'
#' @examples
#' \dontrun{
#' results <- fit_models_by_group(data, outcomes, predictors)
#' coefs <- extract_raw_coefficients(results, term_pattern = "treatment")
#' }
#' @export
extract_raw_coefficients <- function(model_results, term_pattern = NULL, ci_level = 0.95) {

  validate_params(
    data = model_results,
    columns = "model_res",
    context = "extract_coefficients"
  )

  z_val <- qnorm((1 + ci_level) / 2)

  result <- model_results %>%
    select(-any_of(c("model_obj", "dataf"))) %>%
    unnest(cols = all_of("model_res"))

  if (!is.null(term_pattern)) {
    result <- filter(result, str_detect(.data$term, term_pattern))
  }

  result %>%
    mutate(
      ci_low = .data$estimate - (.data$std.error * z_val),
      ci_high = .data$estimate + (.data$std.error * z_val),
      sig = .data$p.value < (1 - ci_level)
    )
}


#' Extract Standardized Coefficients
#'
#' Extracts standardized coefficients using effectsize::standardize_parameters().
#'
#' @param model_results Tibble from fit_models_by_group() containing model_obj column
#' @param term_pattern Regex pattern to filter terms (optional
#' @param method Standardization method (default: "posthoc"). See ?effectsize::standardize_parameters
#'
#' @return Tibble with standardized coefficients and significance flag
#'
#' @examples
#' \dontrun{
#' results <- fit_models_by_group(data, outcomes, predictors)
#' std_coefs <- extract_standardized_coefs(results, term_pattern = "age")
#' }
#' @export
extract_standardized_coefs <- function(model_results, term_pattern = NULL,
                                       method = "posthoc") {

  validate_params(
    data = model_results,
    columns = "model_obj",
    context = "extract_standardized_coefs"
  )

  if (!requireNamespace("parameters", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg parameters} package required for standardized coefficients",
      "i" = "Install with: {.code install.packages('parameters')}"
    ))
  }

  result <- model_results %>%
    mutate(
      std_params = map(.data$model_obj, function(m) {
        if (is.null(m)) return(NULL)
        parameters::standardize_parameters(m, method = method)
      })
    ) %>%
    select(-any_of(c("model_obj", "model_res", "dataf"))) %>%
    unnest(cols = all_of("std_params"))

  if (!is.null(term_pattern)) {
    result <- filter(result, str_detect(.data$Parameter, term_pattern))
  }

  result %>%
    mutate(sig = sign(.data$CI_low) == sign(.data$CI_high))
}
