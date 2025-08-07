#' Create Interaction Plots from Model Results
#'
#' Generates ggplot2 visualizations for two-way interaction effects between
#' numeric and categorical variables from regression models. Automatically
#' identifies variable types and creates marginal effects plots with optional
#' significance highlighting.
#'
#' @param interaction_df Data frame containing model results with required columns:
#'   \itemize{
#'     \item \code{model_obj}: List column containing fitted model objects compatible
#'       with \code{broom::tidy()} and \code{ggeffects::predict_response()}
#'     \item \code{model_equation}: Character column with model formulas containing
#'       interaction terms (e.g., "outcome ~ var1 * var2 + covariates")
#'     \item \code{outcome}: Character column with outcome variable names
#'     \item \code{dataf}: List column containing data frames used for each model
#'   }
#' @param x_labels Named character vector for custom x-axis labels. Names should
#'   match numeric variable names in the data. Optional.
#' @param y_labels Named character vector for custom y-axis labels. Names should
#'   match outcome variable names. Optional.
#' @param p_threshold Numeric value between 0 and 1 for significance threshold
#'   (default: 0.05). Interactions with p-values below this threshold are highlighted.
#' @param significance_color Character string specifying the border color for
#'   statistically significant plots (default: "red")
#' @param base_theme ggplot2 theme object to use as base (default: \code{theme_bw()}).
#'   Custom themes can be passed to match publication requirements.
#' @param color_values Named character vector of colors for categorical group levels.
#'   Names should match the factor levels in the categorical variable. Optional.
#' @param show_ribbon Logical indicating whether to display confidence bands
#'   around predictions (default: FALSE for cleaner multi-panel displays)
#' @param show_plot_titles Logical indicating whether to display plot titles
#'   showing the interaction (default: TRUE)
#'
#' @return Named list of ggplot2 objects. Names follow the pattern "outcome_numericvar"
#'   for easy identification and subsetting.
#'
#' @details
#' This function is designed to work with model results from \code{fit_models_by_group()}
#' or similar functions that produce a structured data frame of model objects.
#' It automatically:
#' \itemize{
#'   \item Extracts interaction terms from model equations
#'   \item Identifies which variable is numeric vs categorical
#'   \item Generates predicted values across the range of the numeric variable
#'   \item Creates separate lines for each level of the categorical variable
#'   \item Highlights statistically significant interactions with colored borders
#' }
#'
#' @section Limitations:
#' Currently supports only:
#' \itemize{
#'   \item Two-way interactions (var1 * var2)
#'   \item Interactions between one numeric and one categorical variable
#'   \item Models compatible with \code{ggeffects::predict_response()}
#' }
#'
#' @examples
#' \dontrun{
#' # Create models with interactions
#' models_df <- fit_models_by_group(
#'   data = my_data,
#'   outcomes = c("memory", "executive"),
#'   base_predictors = c("age", "sex", "education"),
#'   model_type = "interaction",
#'   interaction_terms = "age * group"
#' )
#'
#' # Generate plots
#' plots <- create_interaction_plots(
#'   models_df,
#'   x_labels = c("age" = "Age (years)"),
#'   y_labels = c("memory" = "Memory Score", "executive" = "Executive Function"),
#'   color_values = c("control" = "blue", "treatment" = "red"),
#'   significance_color = "green"
#' )
#'
#' # Access specific plot
#' plots[["memory_age"]]
#' }
#'
#' @seealso
#' \code{\link{organize_interaction_plots}} for arranging plots in a grid
#' \code{\link{generate_interaction_plots}} for complete pipeline
#' \code{\link{fit_models_by_group}} for creating compatible model data frames
#'
#' @export
create_interaction_plots <- function(interaction_df,
                                     x_labels = NULL,
                                     y_labels = NULL,
                                     p_threshold = 0.05,
                                     significance_color = "yellow",
                                     base_theme = theme_bw(),
                                     color_values = NULL,
                                     show_ribbon = FALSE,
                                     show_plot_titles = TRUE) {

  # Input validation
  validate_params(
    data = interaction_df,
    columns = c("model_obj", "model_equation", "outcome", "dataf"),
    custom_checks = list(
      list(
        condition = is.list(interaction_df$model_obj),
        message = "Column 'model_obj' must be a list column containing model objects"
      ),
      list(
        condition = is.list(interaction_df$dataf),
        message = "Column 'dataf' must be a list column containing data frames"
      ),
      list(
        condition = is.numeric(p_threshold) && p_threshold > 0 && p_threshold < 1,
        message = "p_threshold must be a numeric value between 0 and 1"
      )
    ),
    context = "create_interaction_plots"
  )

  plots <- list()

  for(i in 1:nrow(interaction_df)) {
    if(is.null(interaction_df$model_obj[[i]])) next

    # Get the data for this model
    dataf <- interaction_df$dataf[[i]]

    # Extract interaction terms
    equation <- as.character(interaction_df$model_equation[i])
    interaction <- str_extract(equation, "\\w+\\s*\\*\\s*\\w+")
    vars <- str_split(interaction, "\\s*\\*\\s*")[[1]]

    # Identify numeric vs categorical
    is_numeric <- map_lgl(vars, ~is.numeric(dataf[[.x]]))

    # Validate and assign variables
    if(sum(is_numeric) != 1 || sum(!is_numeric) != 1) {
      warning(paste("Model", i, "requires exactly one numeric and one categorical variable"))
      next
    }

    x_var <- vars[is_numeric]
    group_var <- vars[!is_numeric]

    # Check significance
    is_significant <- interaction_df$model_obj[[i]] %>%
      tidy() %>%
      filter(str_detect(.data$term, ":")) %>%
      pull(.data$p.value) %>%
      {\(x) any(x < p_threshold, na.rm = TRUE)}()

    # Labels
    outcome <- interaction_df$outcome[i]
    y_label <- y_labels[outcome] %||% outcome
    x_label <- x_labels[x_var] %||% x_var

    # Get predictions
    pred_data <- predict_response(interaction_df$model_obj[[i]],
                                  terms = c(x_var, group_var))

    p <- ggplot(pred_data, aes(x = .data$x, y = .data$predicted, color = .data$group))


    if(show_ribbon) {
      p <- p + geom_ribbon(aes(ymin = .data$conf.low, ymax = .data$conf.high,
                               fill = .data$group),
                           alpha = 0.2, color = NA)
    }

    plot_title <- if(show_plot_titles) paste(y_label, "vs", group_var) else NULL

    p <- p +
      geom_line(linewidth = 1) +
      labs(y = y_label, x = x_label, title = plot_title) +
      base_theme +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )

    # Apply colors
    if(!is.null(color_values)) {
      p <- p +
        scale_color_manual(values = color_values) +
        scale_fill_manual(values = color_values)
    }

    # Legend styling
    p <- p + guides(
      fill = "none",
      color = guide_legend(
        override.aes = list(fill = NA, linewidth = 1),
        keywidth = unit(0.6, "cm"),
        keyheight = unit(0.2, "cm")
      )
    )

    # Significance border
    if(is_significant) {
      p <- p + theme(panel.border = element_rect(color = significance_color, linewidth = 1.5))
    }

    plots[[paste(outcome, x_var, sep = "_")]] <- p
  }

  plots
}

#' Organize Interaction Plots into Grid Layout
#'
#' Combines multiple interaction plots into a publication-ready grid layout
#' using the patchwork package. Provides flexible ordering and selection options.
#'
#' @param plots Named list of ggplot2 objects from \code{create_interaction_plots()}
#' @param nrow Integer specifying number of rows in the grid (optional)
#' @param ncol Integer specifying number of columns in the grid (optional)
#' @param outcome_order Character vector specifying the desired order of outcomes (optional)
#' @param numeric_var_order Character vector specifying the desired order of numeric variables (optional)
#' @param plot_range Integer vector specifying which plots to include (optional)
#'
#' @return A patchwork object containing the combined plot grid
#'
#' @export
organize_interaction_plots <- function(plots,
                                       nrow = NULL,
                                       ncol = NULL,
                                       outcome_order = NULL,
                                       numeric_var_order = NULL,
                                       plot_range = NULL) {

  # Input validation
  validate_params(
    custom_checks = list(
      list(
        condition = is.list(plots) && length(plots) > 0,
        message = "plots must be a non-empty list"
      ),
      list(
        condition = all(sapply(plots, function(x) inherits(x, "gg"))),
        message = "All elements in plots must be ggplot objects"
      ),
      list(
        condition = is.null(nrow) || (is.numeric(nrow) && nrow > 0),
        message = "nrow must be a positive integer or NULL"
      ),
      list(
        condition = is.null(ncol) || (is.numeric(ncol) && ncol > 0),
        message = "ncol must be a positive integer or NULL"
      )
    ),
    context = "organize_interaction_plots"
  )

  # Reorder if specified
  if(!is.null(outcome_order) && !is.null(numeric_var_order)) {
    ordered_names <- expand_grid(
      outcome = outcome_order,
      numeric_var = numeric_var_order
    ) %>%
      unite("name", .data$outcome, .data$numeric_var, sep = "_") %>%
      pull(.data$name) %>%
      intersect(names(plots))

    plots <- plots[ordered_names]
  }

  # Apply range
  if(!is.null(plot_range)) {
    valid_range <- plot_range[plot_range <= length(plots)]
    if(length(valid_range) == 0) {
      stop("plot_range does not contain any valid plot indices", call. = FALSE)
    }
    plots <- plots[valid_range]
  }

  wrap_plots(plots, nrow = nrow, ncol = ncol) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = 'A') &
    theme(legend.position = "bottom")
}

#' Generate Interaction Plots Pipeline
#'
#' Convenience function that combines \code{create_interaction_plots()} and
#' \code{organize_interaction_plots()} into a single workflow.
#'
#' @inheritParams organize_interaction_plots
#' @inheritParams create_interaction_plots
#' @param interaction_df Data frame containing model results with model_obj column
#' @param ... Additional arguments passed to \code{create_interaction_plots()}
#'
#' @return A patchwork object containing the combined plot grid
#'
#' @export
generate_interaction_plots <- function(interaction_df,
                                       nrow = NULL,
                                       ncol = NULL,
                                       outcome_order = NULL,
                                       numeric_var_order = NULL,
                                       plot_range = NULL,
                                       ...) {

  plots <- create_interaction_plots(interaction_df, ...)

  organize_interaction_plots(
    plots,
    nrow = nrow,
    ncol = ncol,
    outcome_order = outcome_order,
    numeric_var_order = numeric_var_order,
    plot_range = plot_range
  )
}
