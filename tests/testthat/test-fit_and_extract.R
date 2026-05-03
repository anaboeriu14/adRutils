# Helpers -------------------------------------------------------------------

make_model_data <- function() {
  set.seed(123)
  data.frame(
    age      = rnorm(60, mean = 60, sd = 10),
    sex      = rep(c("M", "F"), 30),
    group    = rep(c("AFR", "EUR"), each = 30),
    memory   = rnorm(60, mean = 50, sd = 5),
    speed    = rnorm(60, mean = 100, sd = 15)
  )
}


# fit_models_by_group ------------------------------------------------------

test_that("fit_models_by_group fits one model per (outcome, group) combination", {
  df  <- make_model_data()
  res <- fit_models_by_group(
    df,
    outcomes        = c("memory", "speed"),
    base_predictors = c("age", "sex"),
    group_col       = "group",
    groups          = c("AFR", "EUR"),
    quiet           = TRUE
  )

  expect_equal(nrow(res), 4)             # 2 outcomes x 2 groups
  expect_true(all(c("outcome", "group", "model_obj", "model_res", "n_obs")
                  %in% names(res)))
  expect_true(all(!is.na(res$n_obs)))    # all four models fit successfully
})

test_that("fit_models_by_group with group_col = NULL runs ungrouped", {
  df  <- make_model_data()
  res <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = "age",
    group_col       = NULL,
    quiet           = TRUE
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$group, "All")
})

test_that("fit_models_by_group records errors instead of aborting on a failed fit", {
  df       <- make_model_data()
  df$bad   <- NA_real_                   # an outcome with no variance / all NA
  res <- fit_models_by_group(
    df,
    outcomes        = "bad",
    base_predictors = "age",
    group_col       = NULL,
    quiet           = TRUE
  )

  expect_equal(nrow(res), 1)
  expect_false(is.na(res$model_error))   # error captured, not propagated
  expect_null(res$model_obj[[1]])
})

test_that("fit_models_by_group interaction model uses pairwise terms", {
  df  <- make_model_data()
  res <- fit_models_by_group(
    df,
    outcomes          = "memory",
    base_predictors   = c("age", "sex"),
    group_col         = NULL,
    model_type        = "interaction",
    interaction_terms = "all_pairwise",
    quiet             = TRUE
  )
  # Pairwise interaction wraps predictors with "(...)^2"
  expect_match(res$predictors, "\\^2")
})


# extract_coefficients -----------------------------------------------------

test_that("extract_coefficients adds conf.low/conf.high and a sig flag", {
  df       <- make_model_data()
  fits     <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = "age",
    group_col       = NULL,
    quiet           = TRUE
  )
  coefs <- extract_coefficients(fits)

  expect_true(all(c("estimate", "std.error", "conf.low", "conf.high", "sig")
                  %in% names(coefs)))
  # Hand-check CI math for the first row: estimate +/- 1.96 * SE
  z <- qnorm(0.975)
  expect_equal(coefs$conf.low[1],  coefs$estimate[1] - z * coefs$std.error[1])
  expect_equal(coefs$conf.high[1], coefs$estimate[1] + z * coefs$std.error[1])

  # sig flag is TRUE iff CI excludes zero
  expect_equal(coefs$sig, sign(coefs$conf.low) == sign(coefs$conf.high))
})

test_that("extract_coefficients term_pattern filters by regex", {
  df    <- make_model_data()
  fits  <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = c("age", "sex"),
    group_col       = NULL,
    quiet           = TRUE
  )
  coefs <- extract_coefficients(fits, term_pattern = "age")

  expect_true(all(grepl("age", coefs$term)))
})

test_that("extract_coefficients respects ci_level for wider/narrower bands", {
  df    <- make_model_data()
  fits  <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = "age",
    group_col       = NULL,
    quiet           = TRUE
  )
  ci90 <- extract_coefficients(fits, ci_level = 0.90)
  ci99 <- extract_coefficients(fits, ci_level = 0.99)

  # 99% CIs are wider than 90% CIs
  expect_true(all((ci99$conf.high - ci99$conf.low) >
                    (ci90$conf.high - ci90$conf.low)))
})


test_that("extract_standardized_coefficients returns standardized parameters with sig flag", {
  skip_if_not_installed("parameters")

  df    <- make_model_data()
  fits  <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = c("age", "sex"),
    group_col       = NULL,
    quiet           = TRUE
  )
  coefs <- extract_standardized_coefficients(fits)

  # parameters::standardize_parameters returns Parameter, Std_Coefficient,
  # CI_low, CI_high; we add sig
  expect_true("sig" %in% names(coefs))
  expect_true("Parameter" %in% names(coefs))
  expect_equal(coefs$sig, sign(coefs$CI_low) == sign(coefs$CI_high))
})

test_that("extract_standardized_coefficients term_pattern filters by Parameter regex", {
  skip_if_not_installed("parameters")

  df    <- make_model_data()
  fits  <- fit_models_by_group(
    df,
    outcomes        = "memory",
    base_predictors = c("age", "sex"),
    group_col       = NULL,
    quiet           = TRUE
  )
  coefs <- extract_standardized_coefficients(fits, term_pattern = "age")

  expect_true(all(grepl("age", coefs$Parameter)))
})
