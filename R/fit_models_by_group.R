#' Fit linear models across outcomes and groups
#'
#' Fits a linear model for every combination of outcome and group, with
#' optional outcome-specific or group-specific covariates. Returns a tibble
#' with one row per model, including the fitted model object, tidy results,
#' and the data subset used.
#'
#' @param data A data frame containing all variables referenced.
#' @param outcomes Character vector of outcome variable names.
#' @param base_predictors Character vector of predictors included in every
#'   model.
#' @param group_col Column name for grouping. Use `NULL` for ungrouped
#'   analysis (`groups = "All"`).
#' @param groups Character vector of group values to analyze. Default
#'   `"All"` runs on the full dataset.
#' @param model_type One of `"main"` or `"interaction"`.
#' @param interaction_terms Used when `model_type = "interaction"`. Either
#'   `"all_pairwise"` (all two-way interactions among predictors) or a
#'   character string of explicit interaction terms.
#' @param outcome_covariates Optional named list. Two supported shapes:
#'
#'   * Outcome-specific: `list(outcome1 = c("cov1", "cov2"))`. The named
#'     covariates are added to models for that outcome, across all groups.
#'   * Group-and-outcome-specific: `list(group1 = list(outcome1 = c("cov1")))`.
#'     The shape is detected automatically.
#' @param group_covariates Optional named list of group-specific covariates,
#'   e.g., `list(group1 = c("cov1"), group2 = c("cov2"))`.
#' @param quiet If `TRUE`, suppress progress messages. Default `FALSE`.
#'
#' @return A tibble with columns: `outcome`, the grouping column (named
#'   after `group_col` or `"group"`), `model`, `predictors`,
#'   `model_equation`, `model_res` (tidy results, list-column), `model_obj`
#'   (fitted model, list-column), `model_error` (error message if fit
#'   failed, otherwise `NA`), `dataf` (data subset used, list-column), and
#'   `n_obs`.
#'
#' @examples
#' \dontrun{
#' fit_models_by_group(
#'   data            = my_data,
#'   outcomes        = c("memory", "attention"),
#'   base_predictors = c("age", "sex"),
#'   group_col       = "ancestry",
#'   groups          = c("AFR", "EUR")
#' )
#' }
#'
#' @export
fit_models_by_group <- function(data, outcomes, base_predictors,
                                group_col          = NULL,
                                groups             = "All",
                                model_type         = "main",
                                interaction_terms  = NULL,
                                outcome_covariates = NULL,
                                group_covariates   = NULL,
                                quiet              = FALSE) {

  validate_args(
    data            = data,
    columns         = c(outcomes, base_predictors),
    outcomes        = is_nonempty_character(),
    base_predictors = is_nonempty_character(),
    groups          = is_nonempty_character(),
    model_type      = is_one_of(c("main", "interaction"))
  )

  cov_info     <- .prepare_covariates(outcome_covariates, group_covariates,
                                      groups, data)
  combinations <- tidyr::expand_grid(outcome = outcomes, group = groups)
  n            <- nrow(combinations)

  if (!quiet) cli::cli_alert_info("Fitting {n} model{?s}...")

  results <- vector("list", n)
  for (i in seq_len(n)) {
    curr_outcome <- combinations$outcome[i]
    curr_group   <- combinations$group[i]

    predictors <- .build_predictors(
      base_predictors, curr_outcome, curr_group,
      outcome_covariates, group_covariates,
      model_type, interaction_terms,
      cov_info$is_group_specific
    )

    results[[i]] <- .fit_lm(curr_outcome, curr_group, predictors,
                            data, group_col, model_type)
  }

  out      <- purrr::list_rbind(results)
  n_failed <- sum(!is.na(out$model_error))

  if (!quiet) {
    if (n_failed > 0L) {
      cli::cli_alert_warning(
        "Fitted {n} model{?s}; {n_failed} failed (see {.field model_error} column)"
      )
    } else {
      cli::cli_alert_success("Fitted {n} model{?s}")
    }
  }

  out
}


#' Fit a single linear model and wrap the result.
#' @keywords internal
#' @noRd
.fit_lm <- function(outcome, group, predictors, data, group_col, model_type) {
  analysis_data <- if (is.null(group_col) || group == "All") {
    data
  } else {
    data[data[[group_col]] == group, ]
  }

  model_equation <- paste(outcome, "~", predictors)
  col_name       <- group_col %||% "group"

  fit_attempt <- tryCatch(
    list(model = stats::lm(stats::as.formula(model_equation),
                           data = analysis_data),
         error = NA_character_),
    error = function(e) {
      list(model = NULL, error = conditionMessage(e))
    }
  )

  model_fit <- fit_attempt$model
  has_model <- !is.null(model_fit)

  tibble::tibble(
    outcome        = outcome,
    !!rlang::sym(col_name) := group,
    model          = model_type,
    predictors     = predictors,
    model_equation = model_equation,
    model_res      = list(if (has_model) broom::tidy(model_fit) else NULL),
    model_obj      = list(model_fit),
    model_error    = fit_attempt$error,
    dataf          = list(if (has_model) analysis_data else NULL),
    n_obs          = if (has_model) stats::nobs(model_fit) else NA_integer_
  )
}


#' Detect whether outcome_covariates is shaped per-group, and validate that
#' all referenced covariates exist in `data`.
#' @keywords internal
#' @noRd
.prepare_covariates <- function(outcome_covariates, group_covariates,
                                groups, data) {
  is_group_specific <- !is.null(outcome_covariates) &&
    all(names(outcome_covariates) %in% groups) &&
    all(vapply(outcome_covariates, is.list, logical(1)))

  all_covs <- unique(c(
    unlist(outcome_covariates, use.names = FALSE),
    unlist(group_covariates,   use.names = FALSE)
  ))

  if (length(all_covs) > 0L) {
    validate_args(data = data, columns = all_covs)
  }

  list(is_group_specific = is_group_specific, all_covariates = all_covs)
}


#' Construct the right-hand side of the model formula for a given
#' outcome/group combination.
#' @keywords internal
#' @noRd
.build_predictors <- function(base_predictors, outcome, group,
                              outcome_covariates, group_covariates,
                              model_type, interaction_terms, is_group_specific) {

  all_predictors <- base_predictors

  if (!is.null(outcome_covariates)) {
    if (is_group_specific && group %in% names(outcome_covariates) &&
        outcome %in% names(outcome_covariates[[group]])) {
      all_predictors <- c(all_predictors, outcome_covariates[[group]][[outcome]])
    } else if (!is_group_specific && outcome %in% names(outcome_covariates)) {
      all_predictors <- c(all_predictors, outcome_covariates[[outcome]])
    }
  }

  if (!is.null(group_covariates) && group %in% names(group_covariates)) {
    all_predictors <- c(all_predictors, group_covariates[[group]])
  }

  all_predictors <- unique(all_predictors)
  main_effects   <- paste(all_predictors, collapse = " + ")

  if (model_type == "main" || is.null(interaction_terms)) {
    return(main_effects)
  }
  if (interaction_terms == "all_pairwise") {
    return(paste0("(", main_effects, ")^2"))
  }
  paste(c(main_effects, interaction_terms), collapse = " + ")
}
