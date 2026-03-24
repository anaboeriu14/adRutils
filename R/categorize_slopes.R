#' Categorize random slopes from a mixed-effects model
#'
#' Extracts random slopes from a fitted `lme4` model, z-scores them, and
#' assigns group labels using SD-based or quantile-based cutoffs.
#'
#' @param model A fitted `lmerMod` object with a random slope term.
#' @param id_col Name for the ID column in the output. Default `"id"`.
#' @param slope_term Name of the random slope variable. Default `"time"`.
#' @param methods Character vector: any of `"1sd"`, `"1.5sd"`, `"tertile"`, `"quartile"`.
#' @param labels Named list with `"low"`, `"mid"`, `"high"` group labels.
#' @param prefix Prefix for output columns (e.g., `group_1sd`). Default `"group"`.
#' @param ref_level Reference level for the output factor. Default is `labels$mid`.
#'
#' @return A tibble with `id`, `random_intercept`, `random_slope`, `slope_z`,
#'   and one factor column per method.
#'
#' @examples
#' \dontrun{
#' model <- lmer(pace ~ time + (1 + time | id), data = long_data)
#' trajectories <- categorize_slopes(model, id_col = "subject_id")
#'
#' # Custom labels
#' categorize_slopes(
#'   model,
#'   labels = list(low = "Decreasing", mid = "Stable", high = "Increasing"),
#'   prefix = "bmi_traj"
#' )
#' }
#'
#' @export
categorize_slopes <- function(model,
                              id_col = "id",
                              slope_term = "time",
                              methods = c("1sd","1.5sd","tertile", "quartile"),
                              labels = list(low = "Slow", mid = "Typical", high = "Fast"),
                              prefix = "group",
                              ref_level = labels$mid) {

  validate_params(
    method = methods[1],
    valid_methods = c("1sd", "1.5sd", "tertile", "quartile"),
    custom_checks = list(
      list(
        condition = inherits(model, "lmerMod"),
        message = "{.arg model} must be a {.cls lmerMod} object."
      ),
      list(
        condition = all(c("low", "mid", "high") %in% names(labels)),
        message = "{.arg labels} must have elements: low, mid, high"
      ),
      list(
        condition = all(methods %in% c("1sd", "1.5sd", "tertile", "quartile")),
        message = "All {.arg methods} must be one of: 1sd, 1.5sd, tertile, quartile"
      )
    ),
    context = "categorize_slopes"
  )

  # Extract random effects
  re <- lme4::ranef(model)

  group_factor <- NULL
  for (gf in names(re)) {
    if (slope_term %in% colnames(re[[gf]])) {
      group_factor <- gf
      break
    }
  }

  if (is.null(group_factor)) {
    available <- unlist(lapply(re, colnames))
    cli::cli_abort("Slope term {.val {slope_term}} not found. Available: {.val {available}}")
  }

  re_df <- re[[group_factor]]

  trajectories <- tibble::tibble(!!id_col := rownames(re_df))

  if ("(Intercept)" %in% colnames(re_df)) {
    trajectories$random_intercept <- re_df[["(Intercept)"]]
  }

  trajectories$random_slope <- re_df[[slope_term]]
  trajectories$slope_z <- as.numeric(scale(trajectories$random_slope))

  level_order <- c(ref_level, setdiff(unlist(labels), ref_level))

  for (method in methods) {

    col_name <- paste0(prefix, "_", method)

    trajectories <- trajectories %>%
      dplyr::mutate(
        !!col_name := dplyr::case_when(
          method == "1sd"   & slope_z < -1.0 ~ labels$low,
          method == "1sd"   & slope_z >  1.0 ~ labels$high,
          method == "1sd"                     ~ labels$mid,

          method == "1.5sd" & slope_z < -1.5 ~ labels$low,
          method == "1.5sd" & slope_z >  1.5 ~ labels$high,
          method == "1.5sd"                   ~ labels$mid,

          method == "tertile"  & random_slope <= stats::quantile(random_slope, 0.33) ~ labels$low,
          method == "tertile"  & random_slope >= stats::quantile(random_slope, 0.67) ~ labels$high,
          method == "tertile"                                                        ~ labels$mid,

          method == "quartile" & random_slope <= stats::quantile(random_slope, 0.25) ~ labels$low,
          method == "quartile" & random_slope >= stats::quantile(random_slope, 0.75) ~ labels$high,
          method == "quartile"                                                       ~ labels$mid,

          TRUE ~ NA_character_
        ),
        !!col_name := factor(.data[[col_name]], levels = level_order)
      )
  }

  return(trajectories)
}
