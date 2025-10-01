#' Create Two Way Categorical Interaction Plot from Model Predictions
#'
#' Creates an interaction plot visualizing model predictions from a fitted model for two categorical variables.
#' Predictions represent expected values for each combination of
#' x_var and group_var, averaging over other covariates in the model.
#'
#' @param model A fitted model object (e.g., from lm, lmer) that can be used with emmeans
#' @param x_var Character string specifying the x-axis variable name
#' @param group_var Character string specifying the grouping variable name (for color)
#' @param outcome_name Character string specifying the outcome variable name (optional).
#'   Used to look up label from OUTCOME_LABELS if it exists in the global environment
#' @param outcome_label Character string for the y-axis label. Takes precedence over outcome_name
#' @param data Data frame containing the original data used to fit the model (optional).
#'   Required only if filtering by min_n
#' @param min_n Minimum number of observations required to display a combination (default: NULL).
#'   Only used when data is provided. Set to NULL for no filtering
#' @param color_values Named vector of colors for the grouping variable levels (optional)
#' @param group_label Character string for the legend title. Defaults to group_var if NULL
#' @param x_label Character string for the x-axis label (default: "")
#' @param base_size Base font size for theme_bw (default: 12)
#' @param dodge_width Width for position_dodge (default: 0.75)
#' @param point_size Size of points (default: 2)
#' @param linewidth Width of error bars (default: 0.8)
#'
#' @return A ggplot object showing the predicted values with confidence intervals
#'   and confidence intervals
#'
#' @details
#' If both data and min_n are provided, the function filters out combinations
#' with fewer than min_n observations. This is a visualization decision only -
#' all data is still used in the model fitting and estimation.
#'
#'
#' @examples
#' \dontrun{
#' # Basic usage without filtering
#' model <- lm(outcome ~ age + sex + bmi + group * treatment, data = mydata)
#' plot_emmeans_interaction(model, x_var = "treatment", group_var = "group")
#'
#' # With filtering and custom labels
#' plot_emmeans_interaction(
#'   model = model,
#'   x_var = "treatment",
#'   group_var = "group",
#'   outcome_label = "Blood Pressure (mmHg)",
#'   data = mydata,
#'   min_n = 10,
#'   color_values = c("Control" = "blue", "Treatment" = "red")
#' )
#' }
#'
#' @export
plot_emmeans_interaction <- function(model,
                                     x_var,
                                     group_var,
                                     outcome_name = NULL,
                                     outcome_label = NULL,
                                     data = NULL,
                                     min_n = NULL,
                                     color_values = NULL,
                                     group_label = NULL,
                                     x_label = "",
                                     base_size = 12,
                                     dodge_width = 0.75,
                                     point_size = 2,
                                     linewidth = 0.8) {

  # Determine outcome label
  y_label <- if (!is.null(outcome_label)) outcome_label
  else if (!is.null(outcome_name)) outcome_name
  else ""

  # Validate if data provided
  if (!is.null(data)) {
    # Build columns list conditionally
    cols <- c(x_var, group_var)
    numeric_cols <- NULL

    if (!is.null(outcome_name)) {
      cols <- c(cols, outcome_name)
      numeric_cols <- outcome_name
    }

    validate_params(
      data = data,
      columns = cols,
      numeric_columns = numeric_cols,
      grouping_vars = c(x_var, group_var),
      custom_checks = list(
        list(
          condition = is.null(min_n) || is.numeric(min_n),
          message = "min_n must be numeric or NULL"
        ),
        list(
          condition = is.null(min_n) || min_n >= 1,
          message = "min_n must be at least 1"
        )
      ),
      context = "plot_emmeans_interaction"
    )
  }

  # Warn if min_n set without data
  if (is.null(data) && !is.null(min_n)) {
    cli::cli_warn("Parameter {.arg min_n} is ignored when {.arg data} is NULL")
  }

  # Get emmeans
  emm_df <- emmeans(model, as.formula(paste("~", x_var, "*", group_var))) %>%
    suppressWarnings() %>%
    as.data.frame()

  # Filter by min_n only if BOTH data and min_n are provided
  if (!is.null(data) && !is.null(min_n)) {
    data_combos <- data %>%
      filter(!is.na(.data[[x_var]]), !is.na(.data[[group_var]])) %>%
      count(across(all_of(c(x_var, group_var))), name = "n_obs")

    emm_plot_df <- emm_df %>%
      left_join(data_combos, by = c(x_var, group_var)) %>%
      mutate(n_obs = replace_na(.data$n_obs, 0)) %>%
      filter(.data$n_obs >= min_n)
  }

  # Create plot
  ggplot(emm_plot_df, aes(x = .data[[x_var]], y = .data$emmean,
                     color = .data[[group_var]], group = .data[[group_var]])) +
    geom_point(size = point_size, position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
                   linewidth = linewidth,
                   position = position_dodge(width = dodge_width)) +
    {if (!is.null(color_values)) scale_color_manual(values = color_values,
                                                    name = group_label %||% group_var)} +
    theme_bw(base_size = base_size) +
    labs(y = if (y_label != "") paste("Predicted\n", y_label) else "Predicted",
         x = x_label,
         color = group_label %||% group_var) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
}
