# classify_trajectory_groups requires lme4. We build a small mixed-effects
# model fixture in a helper and skip if lme4 isn't available.

make_traj_model <- function() {
  skip_if_not_installed("lme4")
  set.seed(42)

  # 30 subjects, 4 timepoints each, varying slopes
  n_subj <- 30
  long <- data.frame(
    subject_id = rep(seq_len(n_subj), each = 4),
    time       = rep(0:3, n_subj),
    score      = NA_real_
  )
  # Inject heterogeneous slopes so categorization isn't degenerate
  subj_slopes <- rnorm(n_subj, mean = 0, sd = 1)
  long$score  <- 50 +
    subj_slopes[long$subject_id] * long$time +
    rnorm(nrow(long), sd = 0.5)

  lme4::lmer(score ~ time + (1 + time | subject_id), data = long)
}


test_that("classify_trajectory_groups returns one row per random-effects group with required columns", {
  fit <- make_traj_model()
  res <- classify_trajectory_groups(fit, id_col = "subject_id")

  expect_equal(nrow(res), 30)
  expect_true(all(c("subject_id", "random_intercept", "random_slope", "slope_z")
                  %in% names(res)))
})

test_that("classify_trajectory_groups creates one factor column per requested method", {
  fit <- make_traj_model()
  res <- classify_trajectory_groups(fit, id_col = "subject_id",
                                    methods = c("1sd", "tertile"))

  expect_true("group_1sd"     %in% names(res))
  expect_true("group_tertile" %in% names(res))
  expect_s3_class(res$group_1sd,     "factor")
  expect_s3_class(res$group_tertile, "factor")
})

test_that("classify_trajectory_groups uses mid as the reference level by default", {
  fit <- make_traj_model()
  res <- classify_trajectory_groups(fit, id_col = "subject_id",
                                    methods = "1sd")

  expect_equal(levels(res$group_1sd)[1], "Typical")
})

test_that("classify_trajectory_groups respects custom labels and prefix", {
  fit <- make_traj_model()
  res <- classify_trajectory_groups(
    fit,
    id_col  = "subject_id",
    methods = "1sd",
    labels  = list(low = "Decliner", mid = "Stable", high = "Improver"),
    prefix  = "cog"
  )

  expect_true("cog_1sd" %in% names(res))
  expect_setequal(levels(res$cog_1sd), c("Decliner", "Stable", "Improver"))
})

test_that("classify_trajectory_groups errors on bad methods", {
  fit <- make_traj_model()
  expect_error(
    classify_trajectory_groups(fit, methods = "bogus"),
    regexp = "subset of"
  )
})

test_that("classify_trajectory_groups errors on non-lme4 model", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(classify_trajectory_groups(fit), regexp = "lmerMod")
})
