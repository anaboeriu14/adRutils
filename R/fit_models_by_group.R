#' Fit Linear Models Across Outcomes and Groups
#'
#' Fits linear models for multiple outcomes across different groups with flexible
#' covariate specifications. This function is specifically designed for linear regression
#' and uses lm() internally.
#'
#' @param data A data frame containing the variables for analysis
#' @param outcomes Character vector of outcome variable names to model
#' @param base_predictors Character vector of predictor variable names to include in all models
#' @param group_col Character string specifying the grouping column. Default is "superpop"
#' @param groups Character vector specifying which groups to analyze. Use "All" for all data. Default is "All"
#' @param model_type Character string: "main" or "interaction". Default is "main"
#' @param interaction_terms Character vector of interaction terms when model_type = "interaction"
#' @param outcome_covariates Optional outcome-specific covariates. Can be:
#'   - Standard: list(outcome1 = c("X1", "X2"))
#'   - Group-specific: list(group1 = list(Y1 = c("X2")))
#' @param group_covariates Optional group-specific covariates: list(group1 = c("X1"))
#'
#' @return A tibble with columns: outcome, superpop, model, predictors, model_equation, model_res, dataf, n_obs

#' @details This function fits linear models using lm() only. For logistic regression,
#'   mixed models, or other model types, use appropriate specialized functions.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory", "attention"),
#'   base_predictors = c("age", "sex"),
#'   groups = c("Group1", "Group2")
#' )
#'
#' # With group-specific outcome covariates
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory"),
#'   base_predictors = c("age", "sex"),
#'   groups = c("AFR", "EUR"),
#'   outcome_covariates = list(
#'     "AFR" = list("memory" = c("education")),
#'     "EUR" = list("memory" = c("education", "language"))
#'   )
#' )
#' }
#' @export
fit_models_by_group <- function(data,
                                outcomes,
                                base_predictors,
                                group_col = "superpop",
                                groups = "All",
                                model_type = "main",
                                interaction_terms = NULL,
                                outcome_covariates = NULL,
                                group_covariates = NULL) {

  # Detect outcome_covariates structure first (needed for validation)
  is_group_specific <- .is_group_specific_structure(outcome_covariates, groups)

  # Get all potential covariate columns for validation
  all_potential_covs <- .extract_all_covariates(outcome_covariates, group_covariates, is_group_specific)

  # Comprehensive validation
  validate_params(
    data = data,
    columns = c(group_col, outcomes, base_predictors, all_potential_covs),
    grouping_vars = if(any(groups != "All")) group_col else NULL,
    method = model_type,
    valid_methods = c("main", "interaction"),
    custom_checks = list(
      list(
        condition = is.character(outcomes) && length(outcomes) > 0,
        message = "outcomes must be a non-empty character vector"
      ),
      list(
        condition = is.character(base_predictors) && length(base_predictors) > 0,
        message = "base_predictors must be a non-empty character vector"
      ),
      list(
        condition = is.character(groups) && length(groups) > 0,
        message = "groups must be a non-empty character vector"
      ),
      list(
        condition = if(model_type == "interaction") {
          is.null(interaction_terms) || is.character(interaction_terms)
        } else TRUE,
        message = "interaction_terms must be NULL or a character vector when model_type = 'interaction'"
      )
    ),
    context = "fit_models_by_group"
  )

  # Create all outcome-group combinations
  combinations <- expand_grid(outcome = outcomes, group = groups)

  # Fit models for each combination
  results <- map(1:nrow(combinations), function(i) {
    curr_outcome <- combinations$outcome[i]
    curr_group <- combinations$group[i]

    # Build predictors for this specific outcome-group combo
    predictors <- .build_predictors(
      base_predictors = base_predictors,
      outcome = curr_outcome,
      group = curr_group,
      outcome_covariates = outcome_covariates,
      group_covariates = group_covariates,
      model_type = model_type,
      interaction_terms = interaction_terms,
      is_group_specific = is_group_specific
    )

    # Fit the model
    fit_single_lm(curr_outcome, curr_group, predictors, data, group_col, model_type)
  }) %>% list_rbind()

  return(results)
}

#' Fit a Single Linear Model for One Outcome-Group Combination
#'
#' Fits a single linear model (lm) for a specific outcome within a specific group.
#' This is the core modeling function used by fit_models_by_group(), but can also
#' be used standalone for individual model fitting.
#'
#' @param outcome Character string specifying the outcome variable name
#' @param group Character string specifying the group value, or "All" for all data
#' @param predictors Character string containing the predictor formula (e.g., "age + sex + treatment")
#' @param data A data frame containing the variables
#' @param group_col Character string specifying the grouping column name
#' @param model_type Character string specifying model type ("main" or "interaction")
#'
#' @return A single-row tibble with columns: outcome, superpop, model, predictors,
#'   model_equation, model_res (tidy results), dataf (analysis data), n_obs
#'
#' @details This function only fits linear models using lm(). For other model types
#'   (glm, mixed models, etc.), use other functions.
#'
#' @examples
#' \dontrun{
#' # Fit single model
#' result <- fit_single_lm(
#'   outcome = "memory_score",
#'   group = "EUR",
#'   predictors = "age + sex + education",
#'   data = my_data,
#'   group_col = "ancestry"
#' )
#'
#' # Extract the fitted model
#' model <- result$res[[1]]
#' summary(model)
#' }
#' @export
fit_single_lm <- function(outcome, group, predictors, data, group_col = "superpop", model_type = "main") {

  # Filter data for the group
  if (group == "All") {
    analysis_data <- data
  } else {
    analysis_data <- data %>% filter(.data[[group_col]] == group)
  }

  # Create model equation
  model_equation <- paste(outcome, "~", predictors)

  # Fit model with error handling
  model_fit <- tryCatch({
    lm(as.formula(model_equation), data = analysis_data)
  }, error = function(e) {
    warning("Model failed for ", outcome, " in group ", group, ": ", e$message)
    return(NULL)
  })

  # Return results
  if (is.null(model_fit)) {
    tibble(
      outcome = outcome,
      superpop = group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      model_res = list(NULL),
      model_obj = list(NULL),
      dataf = list(NULL),
      n_obs = NA_integer_
    )
  } else {
    tibble(
      outcome = outcome,
      superpop = group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      model_res = list(tidy(model_fit)),
      model_obj = list(model_fit),
      dataf = list(analysis_data),
      n_obs = nobs(model_fit)
    )
  }
}


#' Extract all covariate names for validation
#' @keywords internal
.extract_all_covariates <- function(outcome_covariates, group_covariates, is_group_specific) {
  all_covs <- character(0)

  # Extract from outcome_covariates
  if (!is.null(outcome_covariates)) {
    all_covs <- c(all_covs, unlist(outcome_covariates, use.names = FALSE))
  }

  # Extract from group_covariates
  if (!is.null(group_covariates)) {
    all_covs <- c(all_covs, unlist(group_covariates, use.names = FALSE))
  }

  return(unique(all_covs))
}

#' Check if outcome_covariates uses group-specific structure
#' @keywords internal
.is_group_specific_structure <- function(outcome_covariates, groups) {
  if (is.null(outcome_covariates)) return(FALSE)

  # Group-specific if: all names are groups AND all values are lists
  all(names(outcome_covariates) %in% groups) && all(sapply(outcome_covariates, is.list))
}

#' Build predictor formula string
#' @keywords internal
.build_predictors <- function(base_predictors, outcome, group,
                              outcome_covariates, group_covariates,
                              model_type, interaction_terms, is_group_specific) {

  # Start with base predictors
  all_predictors <- base_predictors

  # Add outcome-specific covariates
  if (!is.null(outcome_covariates)) {
    if (is_group_specific) {
      # Group-specific: outcome_covariates[[group]][[outcome]]
      if (group %in% names(outcome_covariates) &&
          outcome %in% names(outcome_covariates[[group]])) {
        all_predictors <- c(all_predictors, outcome_covariates[[group]][[outcome]])
      }
    } else {
      # Standard: outcome_covariates[[outcome]]
      if (outcome %in% names(outcome_covariates)) {
        all_predictors <- c(all_predictors, outcome_covariates[[outcome]])
      }
    }
  }

  # Add group-specific covariates
  if (!is.null(group_covariates) && group %in% names(group_covariates)) {
    all_predictors <- c(all_predictors, group_covariates[[group]])
  }

  # Remove duplicates
  all_predictors <- unique(all_predictors)

  # Build formula based on model type
  if (model_type == "main") {
    return(paste(all_predictors, collapse = " + "))
  } else {
    # Interaction model
    main_effects <- paste(all_predictors, collapse = " + ")

    if (is.null(interaction_terms)) {
      return(main_effects)
    } else if (interaction_terms == "all_pairwise") {
      return(paste("(", main_effects, ")^2"))
    } else {
      return(paste(c(main_effects, interaction_terms), collapse = " + "))
    }
  }
}

