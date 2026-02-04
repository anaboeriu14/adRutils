#' Fit Linear Models Across Outcomes and Groups
#'
#' Fits linear models for multiple outcomes across groups with flexible covariate
#' specifications.
#'
#' @param data Data frame containing variables for analysis
#' @param outcomes Character vector of outcome variable names
#' @param base_predictors Character vector of predictors to include in all models
#' @param group_col Column name for grouping (NULL for no grouping)
#' @param groups Groups to analyze; use "All" for entire dataset (default)
#' @param model_type Either "main" or "interaction"
#' @param interaction_terms Interaction terms when model_type = "interaction"
#' @param outcome_covariates Outcome-specific covariates as named list
#' @param group_covariates Group-specific covariates as named list
#' @param verbose Show progress (default: FALSE)
#'
#' @return Tibble with columns: outcome, group (or group_col name), model,
#'   predictors, model_equation, model_res, model_obj, dataf, n_obs
#'
#' @examples
#' \dontrun{
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory", "attention"),
#'   base_predictors = c("age", "sex"),
#'   group_col = "ancestry",
#'   groups = c("AFR", "EUR")
#' )
#' }
#' @export
fit_models_by_group <- function(data, outcomes, base_predictors,
                                group_col = NULL, groups = "All",
                                model_type = "main", interaction_terms = NULL,
                                outcome_covariates = NULL, group_covariates = NULL,
                                verbose = FALSE) {

  .validate_model_inputs(data, outcomes, base_predictors, groups,
                         model_type, interaction_terms)

  covariate_info <- .prepare_covariates(outcome_covariates, group_covariates,
                                        groups, data)

  combinations <- expand_grid(outcome = outcomes, group = groups)

  if (verbose) {
    cli::cli_progress_bar("Fitting models", total = nrow(combinations))
  }

  results <- map(seq_len(nrow(combinations)), function(i) {
    if (verbose) cli::cli_progress_update()

    curr_outcome <- combinations$outcome[i]
    curr_group <- combinations$group[i]

    predictors <- .build_predictors(
      base_predictors, curr_outcome, curr_group,
      outcome_covariates, group_covariates,
      model_type, interaction_terms,
      covariate_info$is_group_specific
    )

    .fit_lm(curr_outcome, curr_group, predictors, data, group_col, model_type)
  }) %>% list_rbind()

  if (verbose) cli::cli_progress_done()

  results
}

# Fit single linear model
#' @keywords internal
#' @noRd
.fit_lm <- function(outcome, group, predictors, data, group_col, model_type) {

  # Filter data
  if (is.null(group_col) || group == "All") {
    analysis_data <- data
  } else {
    analysis_data <- data[data[[group_col]] == group, ]
  }

  model_equation <- paste(outcome, "~", predictors)
  col_name <- group_col %||% "group"

  model_fit <- tryCatch(
    lm(as.formula(model_equation), data = analysis_data),
    error = function(e) {
      cli::cli_warn("Model failed for {outcome} / {group}: {e$message}")
      NULL
    }
  )

  tibble(
    outcome = outcome,
    !!sym(col_name) := group,
    model = model_type,
    predictors = predictors,
    model_equation = model_equation,
    model_res = list(if (!is.null(model_fit)) tidy(model_fit) else NULL),
    model_obj = list(model_fit),
    dataf = list(if (!is.null(model_fit)) analysis_data else NULL),
    n_obs = if (!is.null(model_fit)) nobs(model_fit) else NA_integer_
  )
}

# Validate inputs
#' @keywords internal
#' @noRd
.validate_model_inputs <- function(data, outcomes, base_predictors, groups,
                                   model_type, interaction_terms) {
  validate_params(
    data = data,
    columns = c(outcomes, base_predictors),
    method = model_type,
    valid_methods = c("main", "interaction"),
    custom_checks = list(
      list(
        condition = is.character(outcomes) && length(outcomes) > 0,
        message = "{.arg outcomes} must be a non-empty character vector"
      ),
      list(
        condition = is.character(base_predictors) && length(base_predictors) > 0,
        message = "{.arg base_predictors} must be a non-empty character vector"
      ),
      list(
        condition = is.character(groups) && length(groups) > 0,
        message = "{.arg groups} must be a non-empty character vector"
      )
    ),
    context = "fit_models_by_group"
  )
}

# Prepare covariates
#' @keywords internal
#' @noRd
.prepare_covariates <- function(outcome_covariates, group_covariates, groups, data) {

  is_group_specific <- FALSE
  if (!is.null(outcome_covariates)) {
    is_group_specific <- all(names(outcome_covariates) %in% groups) &&
      all(sapply(outcome_covariates, is.list))
  }

  all_covs <- c(
    unlist(outcome_covariates, use.names = FALSE),
    unlist(group_covariates, use.names = FALSE)
  ) %>% unique()

  if (length(all_covs) > 0) {
    validate_params(data = data, columns = all_covs,
                    context = "fit_models_by_group (covariates)")
  }

  list(is_group_specific = is_group_specific, all_covariates = all_covs)
}

# Build predictor formula
#' @keywords internal
#' @noRd
.build_predictors <- function(base_predictors, outcome, group,
                              outcome_covariates, group_covariates,
                              model_type, interaction_terms, is_group_specific) {

  all_predictors <- base_predictors

  # Add outcome-specific covariates
  if (!is.null(outcome_covariates)) {
    if (is_group_specific && group %in% names(outcome_covariates) &&
        outcome %in% names(outcome_covariates[[group]])) {
      all_predictors <- c(all_predictors, outcome_covariates[[group]][[outcome]])
    } else if (!is_group_specific && outcome %in% names(outcome_covariates)) {
      all_predictors <- c(all_predictors, outcome_covariates[[outcome]])
    }
  }

  # Add group-specific covariates
  if (!is.null(group_covariates) && group %in% names(group_covariates)) {
    all_predictors <- c(all_predictors, group_covariates[[group]])
  }

  all_predictors <- unique(all_predictors)
  main_effects <- paste(all_predictors, collapse = " + ")

  if (model_type == "main" || is.null(interaction_terms)) {
    return(main_effects)
  }

  if (interaction_terms == "all_pairwise") {
    paste0("(", main_effects, ")^2")
  } else {
    paste(c(main_effects, interaction_terms), collapse = " + ")
  }
}
