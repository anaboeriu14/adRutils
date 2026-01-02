#' Fit Linear Models Across Outcomes and Groups
#'
#' Fits linear models for multiple outcomes across different groups with flexible
#' covariate specifications. This function is specifically designed for linear regression
#' and uses lm() internally.
#'
#' @param data A data frame containing the variables for analysis
#' @param outcomes Character vector of outcome variable names to model
#' @param base_predictors Character vector of predictor variable names to include in all models
#' @param group_col Character string specifying the grouping column name.
#'   If NULL, no grouping is applied. Default is NULL
#' @param groups Character vector specifying which groups to analyze. Use "All" for all data. Default is "All"
#' @param model_type Character string: "main" or "interaction". Default is "main"
#' @param interaction_terms Character vector of interaction terms when model_type = "interaction"
#' @param outcome_covariates Optional outcome-specific covariates. Can be:
#'   - Standard: list(outcome1 = c("X1", "X2"))
#'   - Group-specific: list(group1 = list(Y1 = c("X2")))
#' @param group_covariates Optional group-specific covariates: list(group1 = c("X1"))
#' @param verbose Logical. If TRUE, shows progress during model fitting. Default is FALSE
#'
#' @return A tibble with columns: outcome, (group_col name or "group"), model, predictors,
#'   model_equation, model_res (tidy results), model_obj (model object), dataf (analysis data),
#'   n_obs. The grouping column will be named according to the \code{group_col} parameter,
#'   or "group" if group_col is NULL.
#'
#' @details This function fits linear models using lm() only. It calls
#'   \code{\link{fit_single_lm}} internally for each outcome-group combination.
#'
#' @seealso \code{\link{fit_single_lm}} for fitting individual models
#'
#' @examples
#' \dontrun{
#' # Basic usage with ancestry grouping
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory", "attention"),
#'   base_predictors = c("age", "sex"),
#'   group_col = "ancestry",
#'   groups = c("AFR", "EUR")
#' )
#'
#' # With outcome-specific covariates
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory", "attention"),
#'   base_predictors = c("age", "sex"),
#'   outcome_covariates = list(
#'     memory = c("education"),
#'     attention = c("language")
#'   )
#' )
#'
#' # With group-specific outcome covariates
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory"),
#'   base_predictors = c("age", "sex"),
#'   group_col = "ancestry",
#'   groups = c("AFR", "EUR"),
#'   outcome_covariates = list(
#'     AFR = list(memory = c("education")),
#'     EUR = list(memory = c("education", "language"))
#'   )
#' )
#'
#' # With progress reporting
#' results <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = paste0("cog_", 1:20),
#'   base_predictors = c("age", "sex"),
#'   group_col = "ancestry",
#'   groups = c("AFR", "EUR", "EAS"),
#'   verbose = TRUE
#' )
#' }
#' @export
fit_models_by_group <- function(data, outcomes, base_predictors,
                                group_col = NULL, groups = "All",
                                model_type = "main", interaction_terms = NULL,
                                outcome_covariates = NULL, group_covariates = NULL,
                                verbose = FALSE) {
  # Validate basic inputs
  .validate_model_inputs(
    data, outcomes, base_predictors, groups,
    model_type, interaction_terms, verbose
  )

  # Process and validate covariates
  covariate_info <- .prepare_covariates(
    outcome_covariates, group_covariates,
    groups, data, group_col
  )

  # Create outcome-group combinations
  combinations <- expand_grid(outcome = outcomes, group = groups)

  # Progress setup
  if (verbose) {
    cli::cli_progress_bar(
      "Fitting models",
      total = nrow(combinations),
      format = "{cli::pb_spin} Fitting {cli::pb_current}/{cli::pb_total} models"
    )
  }

  # Fit models
  results <- map(1:nrow(combinations), function(i) {
    if (verbose) cli::cli_progress_update()

    curr_outcome <- combinations$outcome[i]
    curr_group <- combinations$group[i]

    predictors <- .build_predictors(
      base_predictors = base_predictors,
      outcome = curr_outcome,
      group = curr_group,
      outcome_covariates = outcome_covariates,
      group_covariates = group_covariates,
      model_type = model_type,
      interaction_terms = interaction_terms,
      is_group_specific = covariate_info$is_group_specific
    )

    fit_single_lm(
      curr_outcome, curr_group, predictors, data,
      group_col, model_type
    )
  }) %>% list_rbind()

  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success(
      "Fitted {nrow(results)} model{?s} across {length(outcomes)} outcome{?s} and {length(groups)} group{?s}"
    )
  }

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
#' @param group_col Character string specifying the grouping column name. If NULL,
#'   the output will use "group" as the column name. Default is NULL
#' @param model_type Character string specifying model type ("main" or "interaction")
#'
#' @return A single-row tibble with columns: outcome, (group_col name or "group"),
#'   model, predictors, model_equation, model_res (tidy results), model_obj (model object),
#'   dataf (analysis data), n_obs. The grouping column name matches the \code{group_col}
#'   parameter, or is named "group" if group_col is NULL.
#'
#' @details This function only fits linear models using lm(). It is called internally
#'   by \code{\link{fit_models_by_group}} but is also exported for standalone use.
#'
#' @seealso \code{\link{fit_models_by_group}} for fitting multiple models at once
#'
#' @examples
#' \dontrun{
#' # Fit single model with ancestry grouping
#' result <- fit_single_lm(
#'   outcome = "memory_score",
#'   group = "EUR",
#'   predictors = "age + sex + education",
#'   data = my_data,
#'   group_col = "ancestry"
#' )
#'
#' # Extract the fitted model
#' model <- result$model_obj[[1]]
#' summary(model)
#'
#' # Access the grouping column (will be named "ancestry")
#' result$ancestry
#'
#' # Fit model on all data (no grouping)
#' result <- fit_single_lm(
#'   outcome = "memory_score",
#'   group = "All",
#'   predictors = "age + sex",
#'   data = my_data
#' )
#' # Output will have a "group" column with value "All"
#' }
#' @export
fit_single_lm <- function(outcome, group, predictors, data,
                          group_col = NULL, model_type = "main") {
  # Validate inputs
  validate_params(
    data = data,
    columns = outcome,
    custom_checks = list(
      list(
        condition = is.character(outcome) && length(outcome) == 1,
        message = "{.arg outcome} must be a single character string"
      ),
      list(
        condition = is.character(group) && length(group) == 1,
        message = "{.arg group} must be a single character string"
      ),
      list(
        condition = is.character(predictors) && length(predictors) == 1,
        message = "{.arg predictors} must be a single character string"
      ),
      list(
        condition = is.null(group_col) || (is.character(group_col) && length(group_col) == 1),
        message = "{.arg group_col} must be NULL or a single character string"
      )
    ),
    context = "fit_single_lm"
  )

  # Filter data for the group
  if (is.null(group_col) || group == "All") {
    analysis_data <- data
  } else {
    # Validate group_col exists
    validate_params(
      data = data,
      columns = group_col,
      context = "fit_single_lm (group_col)"
    )

    analysis_data <- data %>% filter(.data[[group_col]] == group)
  }

  # Create model equation
  model_equation <- paste(outcome, "~", predictors)

  # Fit model with error handling
  model_fit <- tryCatch(
    {
      lm(as.formula(model_equation), data = analysis_data)
    },
    error = function(e) {
      cli::cli_alert_warning("Model failed for {outcome} in group {group}: {e$message}")
      return(NULL)
    }
  )

  # Determine column name for grouping variable
  col_name <- if (is.null(group_col)) "group" else group_col

  # Return results
  if (is.null(model_fit)) {
    return(tibble(
      outcome = outcome,
      !!rlang::sym(col_name) := group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      model_res = list(NULL),
      model_obj = list(NULL),
      dataf = list(NULL),
      n_obs = NA_integer_
    ))
  } else {
    return(tibble(
      outcome = outcome,
      !!rlang::sym(col_name) := group,
      model = model_type,
      predictors = predictors,
      model_equation = model_equation,
      model_res = list(tidy(model_fit)),
      model_obj = list(model_fit),
      dataf = list(analysis_data),
      n_obs = nobs(model_fit)
    ))
  }
}

#' Validate basic model inputs
#' @keywords internal
.validate_model_inputs <- function(data, outcomes, base_predictors, groups,
                                   model_type, interaction_terms, verbose) {
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
      ),
      list(
        condition = if (model_type == "interaction") {
          is.null(interaction_terms) || is.character(interaction_terms)
        } else {
          TRUE
        },
        message = "{.arg interaction_terms} must be NULL or character vector when model_type = 'interaction'"
      ),
      list(
        condition = is.logical(verbose) && length(verbose) == 1,
        message = "{.arg verbose} must be a single logical value"
      )
    ),
    context = "fit_models_by_group"
  )

  invisible(TRUE)
}

#' Prepare and validate covariate specifications
#' @keywords internal
.prepare_covariates <- function(outcome_covariates, group_covariates,
                                groups, data, group_col) {
  # Detect covariate structure
  is_group_specific <- .is_group_specific_structure(outcome_covariates, groups)

  # Extract all covariate columns
  all_covs <- .extract_all_covariates(outcome_covariates, group_covariates)

  # Validate all columns exist in data
  if (length(all_covs) > 0) {
    validate_params(
      data = data,
      columns = all_covs,
      context = "fit_models_by_group (covariates)"
    )
  }

  return(list(
    is_group_specific = is_group_specific,
    all_covariates = all_covs
  ))
}

#' Check if outcome_covariates uses group-specific structure
#' @keywords internal
.is_group_specific_structure <- function(outcome_covariates, groups) {
  if (is.null(outcome_covariates)) {
    return(FALSE)
  }

  # Check if structure is group-specific (nested by group then outcome)
  names_are_groups <- all(names(outcome_covariates) %in% groups)
  values_are_lists <- all(sapply(outcome_covariates, is.list))

  return(names_are_groups && values_are_lists)
}

#' Extract all covariate names for validation
#' @keywords internal
.extract_all_covariates <- function(outcome_covariates, group_covariates) {
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
