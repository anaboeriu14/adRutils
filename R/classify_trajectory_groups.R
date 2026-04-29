#' Classify subjects into trajectory groups from a mixed-effects model
#'
#' Extracts random slopes from a fitted `lme4` model, z-scores them, and
#' assigns each subject to a trajectory group (low / mid / high) using
#' SD-based or quantile-based cutoffs.
#'
#' @details
#' SD-based methods (`"1sd"`, `"1.5sd"`) threshold the z-scored slope:
#' subjects beyond ±1 (or ±1.5) SD from the group mean are assigned to the
#' low/high groups, the rest to mid.
#'
#' Quantile-based methods (`"tertile"`, `"quartile"`) threshold the raw slope:
#' the lowest tertile (or quartile) goes to `low`, the highest to `high`,
#' the rest to `mid`. Subjects exactly at a cutpoint are assigned to the
#' middle group.
#'
#' The `mid` label is the default reference level since it represents the
#' typical trajectory; most downstream contrasts compare extreme groups to
#' the typical one.
#'
#' @param model A fitted `lmerMod` object with a random slope term.
#' @param id_col Output ID column name. Default `"id"`.
#' @param slope_term Name of the random slope variable (must match a column
#'   of `ranef(model)`). Default `"time"`.
#' @param methods Character vector. Any subset of `"1sd"`, `"1.5sd"`,
#'   `"tertile"`, `"quartile"`.
#' @param labels Named list with elements `low`, `mid`, `high`.
#' @param prefix Prefix for output group columns. Default `"group"`.
#' @param ref_level Reference level for the output factor. Default
#'   `labels$mid`.
#'
#' @return A tibble with one row per random-effects group, containing
#'   `<id_col>`, `random_intercept` (if present), `random_slope`, `slope_z`,
#'   and one factor column per requested method, named `<prefix>_<method>`.
#'
#' @examples
#' \dontrun{
#' fit <- lme4::lmer(score ~ time + (1 + time | id), data = long_df)
#' classify_trajectory_groups(fit, id_col = "subject_id")
#'
#' # Custom labels
#' classify_trajectory_groups(
#'   fit,
#'   labels = list(low = "Decliner", mid = "Stable", high = "Improver"),
#'   prefix = "cog_traj"
#' )
#' }
#'
#' @export
classify_trajectory_groups <- function(model,
                                       id_col     = "id",
                                       slope_term = "time",
                                       methods    = c("1sd", "1.5sd", "tertile", "quartile"),
                                       labels     = list(low = "Slow", mid = "Typical", high = "Fast"),
                                       prefix     = "group",
                                       ref_level  = labels$mid) {

  valid_methods <- c("1sd", "1.5sd", "tertile", "quartile")

  validate_args(
    id_col     = is_string(),
    slope_term = is_string(),
    methods    = is_nonempty_character(),
    prefix     = is_string(),
    custom_checks = list(
      list(
        condition = inherits(model, "lmerMod"),
        message   = "{.arg model} must be a {.cls lmerMod} object"
      ),
      list(
        condition = is.list(labels) &&
          all(c("low", "mid", "high") %in% names(labels)),
        message   = "{.arg labels} must be a list with elements: low, mid, high"
      ),
      list(
        condition = all(methods %in% valid_methods),
        message   = paste0("{.arg methods} must be a subset of: ",
                           paste0('"', valid_methods, '"', collapse = ", "))
      ),
      list(
        condition = ref_level %in% unlist(labels),
        message   = "{.arg ref_level} must be one of the {.arg labels} values"
      )
    )
  )

  if (!requireNamespace("lme4", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg lme4} package is required for mixed-effects model utilities",
      "i" = "Install with: {.code install.packages(\"lme4\")}"
    ))
  }

  # Find which random-effects grouping factor contains the slope term
  re       <- lme4::ranef(model)
  slope_in <- vapply(re, function(df) slope_term %in% colnames(df), logical(1))

  if (!any(slope_in)) {
    available <- unlist(lapply(re, colnames))
    cli::cli_abort(c(
      "Slope term {.val {slope_term}} not found in random effects.",
      "i" = "Available terms: {.val {available}}"
    ))
  }
  group_factor <- names(re)[which(slope_in)[1]]
  re_df        <- re[[group_factor]]
  slope_vec    <- re_df[[slope_term]]

  if (stats::var(slope_vec) < .Machine$double.eps^0.5) {
    cli::cli_warn(c(
      "Random slope variance is essentially zero.",
      "i" = "Categorizations will be unstable; check the model with {.fn lme4::isSingular}."
    ))
  }

  trajectories <- tibble::tibble(
    !!id_col     := rownames(re_df),
    random_slope = slope_vec,
    slope_z      = as.numeric(scale(slope_vec))
  )
  if ("(Intercept)" %in% colnames(re_df)) {
    trajectories$random_intercept <- re_df[["(Intercept)"]]
    trajectories <- trajectories[, c(id_col, "random_intercept",
                                     "random_slope", "slope_z")]
  }

  level_order <- c(ref_level, setdiff(unlist(labels), ref_level))
  for (m in methods) {
    col <- paste0(prefix, "_", m)
    trajectories[[col]] <- factor(
      .label_by_method(trajectories$slope_z, trajectories$random_slope, m, labels),
      levels = level_order
    )
  }

  trajectories
}

#' Dispatch to the appropriate labeling function for a given method.
#' @keywords internal
#' @noRd
.label_by_method <- function(z, raw, method, labels) {
  switch(method,
         "1sd"      = .label_by_sd(z, 1.0, labels),
         "1.5sd"    = .label_by_sd(z, 1.5, labels),
         "tertile"  = .label_by_quantile(raw, c(1/3, 2/3), labels),
         "quartile" = .label_by_quantile(raw, c(0.25, 0.75), labels)
  )
}

#' Label by SD-based threshold on z-scored slope.
#' @keywords internal
#' @noRd
.label_by_sd <- function(z, threshold, labels) {
  out <- rep(labels$mid, length(z))
  out[z < -threshold] <- labels$low
  out[z >  threshold] <- labels$high
  out
}

#' Label by quantile cutpoints on raw slope. Lower bucket is strict (<),
#' upper bucket is inclusive (>=); ties go to the middle.
#' @keywords internal
#' @noRd
.label_by_quantile <- function(x, probs, labels) {
  q   <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  out <- rep(labels$mid, length(x))
  out[x <  q[1]] <- labels$low
  out[x >= q[2]] <- labels$high
  out
}
