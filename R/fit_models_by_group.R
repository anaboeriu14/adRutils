#' Fit Linear Models Across Outcomes and Groups
#'
#' Fits linear models for multiple outcomes across different groups (e.g., ancestry, sex,
#' treatment groups, etc.) with flexible covariate specifications and model types.
#' Allows for outcome-specific and group-specific covariates to be added to base predictors.
#'
#' @param data A data frame containing the variables for analysis
#' @param outcomes Character vector of outcome variable names to model
#' @param base_predictors Character vector of predictor variable names to include in all models
#' @param group_col Character string specifying the column name for grouping variable. Default is "superpop"
#' @param groups Character vector specifying which groups to analyze. Use "All" to include all data without grouping. Default is "All"
#' @param model_type Character string specifying the model type. Must be one of: "main", "interaction", "nested", or "stratified".
#'   If NULL, defaults to "main". Default is NULL.
#' @param interaction_terms Character vector specifying which interaction terms to include when model_type = "interaction".
#'   Can use R formula syntax: c("age*sex", "treatment*apoe") includes both main effects and interactions,
#'   or c("age:sex", "treatment:apoe") for interaction-only terms, or "all_pairwise" for all pairwise interactions.
#'   If NULL and model_type = "interaction", will include only main effects (no interactions).
#' @param outcome_covariates Optional named list of additional covariates specific to certain outcomes.
#'   Keys should be outcome names, values should be character vectors of covariate names.
#' @param group_covariates Optional named list of additional covariates specific to certain groups.
#'   Keys should be group names, values should be character vectors of covariate names.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{outcome}{The outcome variable name}
#'   \item{superpop}{The group being analyzed}
#'   \item{model}{The model type specified}
#'   \item{predictors}{The full predictor formula string used}
#'   \item{model_equation}{The complete model equation (outcome ~ predictors)}
#'   \item{res}{List column containing the fitted lm objects}
#'   \item{dataf}{List column containing tidy model results from broom::tidy()}
#'   \item{n_obs}{Number of observations used in each model}
#' }
#'
#' @examples
#' \dontrun{
#' # Main effects model
#' main_results <- fit_group_models(
#'   data = proj_data,
#'   outcomes = c("verbal_ability", "memory"),
#'   base_predictors = c("age", "id_gender", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR"),
#'   model_type = "main"
#' )
#'
#' # Interaction model with specific interactions
#' int_results <- fit_group_models(
#'   data = proj_data,
#'   outcomes = c("tau", "ab42_ab40"),
#'   base_predictors = c("age", "id_gender", "apoe_risk_group"),
#'   groups = c("AFR", "EUR", "AMR"),
#'   model_type = "interaction",
#'   interaction_terms = c("age:id_gender", "age:apoe_risk_group")
#' )
#'
#' # Interaction model with all pairwise interactions
#' full_int_results <- fit_group_models(
#'   data = your_data,
#'   outcomes = c("y"),
#'   base_predictors = c("x1", "x2", "x3"),
#'   groups = c("placebo", "medication"),
#'   model_type = "interaction",
#'   interaction_terms = "all_pairwise"
#' )
#' }
#' @export
fit_models_by_group <- function(data,
                                outcomes,
                                base_predictors,
                                group_col = "superpop",
                                groups = "All",
                                model_type = NULL,
                                interaction_terms = NULL,
                                outcome_covariates = NULL,
                                group_covariates = NULL) {

  # Set default model_type if NULL
  if (is.null(model_type)) model_type <- "main"

  # Get all potential covariate columns for validation
  all_potential_covs <- unique(c(unlist(outcome_covariates),
                                 unlist(group_covariates)
  ))

  validate_params(
    data = data,
    columns = c(group_col, outcomes, base_predictors, all_potential_covs),
    grouping_vars = group_col,
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
        condition = if(!is.null(model_type) && model_type == "interaction") {
          is.null(interaction_terms) || is.character(interaction_terms)
        } else TRUE,
        message = "interaction_terms must be NULL or a character vector when model_type = 'interaction'"
      )
    ),
    context = "fit_group_models"
  )

  combinations <- expand_grid(outcome = outcomes, group = groups)

  results <- map(1:nrow(combinations), function(i) {
    curr_outcome <- combinations$outcome[i]
    curr_group <- combinations$group[i]

    predictors <- .build_predictors(
      base_predictors,
      curr_outcome,
      curr_group,
      outcome_covariates,
      group_covariates,
      model_type,
      interaction_terms
    )

    .fit_single_model(curr_outcome, curr_group, predictors, data, group_col, model_type)
  }) %>%
    list_rbind()

  return(results)
}

# Helper functions (not exported)
#' @keywords internal
.build_predictors <- function(base_predictors, outcome,
                              group, outcome_covariates = NULL,
                              group_covariates = NULL,
                              model_type = "main",
                              interaction_terms = NULL) {

  # Collect all predictors
  all_predictors <- c(
    base_predictors,
    if (!is.null(outcome_covariates) && outcome %in% names(outcome_covariates)) outcome_covariates[[outcome]],
    if (!is.null(group_covariates) && group %in% names(group_covariates)) group_covariates[[group]]
  )

  # Build formula string based on model type
  main_effects <- paste(all_predictors, collapse = " + ")

  formula_str <- switch(model_type,
                        "main" = main_effects,
                        "interaction" = {
                          if (is.null(interaction_terms)) {
                            # No interactions when NULL
                            main_effects
                          } else if (length(interaction_terms) == 1 && interaction_terms == "all_pairwise") {
                            # All pairwise interactions
                            if (length(all_predictors) >= 2) {
                              paste("(", main_effects, ")^2")
                            } else main_effects
                          } else {
                            # Handle both * and :
                            .process_interaction_terms(main_effects, interaction_terms, all_predictors)
                          }
                        }
  )

  return(formula_str)
}

#' Helper function to process interaction terms with * and : syntax
#' @keywords internal
.process_interaction_terms <- function(main_effects, interaction_terms, all_predictors) {

  # Separate * terms (main + interaction) from : terms (interaction only)
  star_terms <- interaction_terms[grepl("\\*", interaction_terms)]
  colon_terms <- interaction_terms[grepl(":", interaction_terms) & !grepl("\\*", interaction_terms)]

  # For : terms, validate that variables are in all_predictors
  if (length(colon_terms) > 0) {
    colon_vars <- unique(unlist(strsplit(colon_terms, ":")))
    invalid_colon_vars <- setdiff(colon_vars, all_predictors)
    if (length(invalid_colon_vars) > 0) {
      warning("Interaction-only terms (:) contain variables not in base_predictors: ",
              paste(invalid_colon_vars, collapse = ", "))
    }
  }

  # For * terms, adds the main effects automatically
  # Just build  formula
  formula_parts <- c(main_effects)

  if (length(star_terms) > 0) {
    formula_parts <- c(formula_parts, star_terms)
  }

  if (length(colon_terms) > 0) {
    formula_parts <- c(formula_parts, colon_terms)
  }

  paste(formula_parts, collapse = " + ")
}

#' @keywords internal
.fit_single_model <- function(outcome, group, predictors,
                              data, group_col, model_type) {

  # Filter data for the specific group
  analysis_data <- if (group == "All") {
    data
  } else {
    data %>% filter(.data[[group_col]] == group)
  }

  # Build model equation
  model_equation <- glue("{outcome} ~ {predictors}")

  # Fit the model with error handling
  model_fit <- tryCatch({
    lm(as.formula(model_equation), data = analysis_data)
  }, error = function(e) {
    warning(paste("Model fitting failed for outcome:", outcome,
                  "group:", group, "- Error:", e$message))
    return(NULL)
  })

  # Create results tibble, for models that failed, res,dataf,n_obs will be NA
  if (is.null(model_fit)) {
    model_res <- tibble(
      outcome = outcome,
      superpop = group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      res = list(NULL),
      dataf = list(NULL),
      n_obs = NA_integer_
    )
  } else {
    model_res <- tibble(
      outcome = outcome,
      superpop = group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      res = list(model_fit),
      dataf = list(tidy(model_fit)),
      n_obs = nobs(model_fit)
    )
  }

  return(model_res)
}
