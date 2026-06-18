# Tests for meta_analysis.R
#
# Note: add_meta_pooled_results requires the metafor package. We skip
# the pooling tests when it's not installed.


# Helpers -------------------------------------------------------------------

make_effects_df <- function() {
  tibble::tribble(
    ~outcome,  ~term,    ~cohort,        ~estimate, ~std.error, ~n_obs,
    "memory",  "ageE4",  "Discovery",    0.30,      0.10,       500L,
    "memory",  "ageE4",  "Replication",  0.25,      0.12,       400L,
    "memory",  "sex",    "Discovery",    0.10,      0.05,       500L,
    "memory",  "sex",    "Replication",  0.08,      0.06,       400L,
    "speed",   "ageE4",  "Discovery",   -0.20,      0.08,       500L,
    "speed",   "ageE4",  "Replication",  0.05,      0.09,       400L     # opposite sign
  )
}


# check_cohort_alignment ----------------------------------------------------

test_that("check_cohort_alignment reports a perfect match when cohorts share keys", {
  x <- data.frame(study = c("A", "A", "B"), term = c("t1", "t2", "t1"))
  y <- data.frame(study = c("A", "A", "B"), term = c("t1", "t2", "t1"))

  res <- check_cohort_alignment(x, y, by = c("study", "term"), quiet = TRUE)

  expect_true(res$perfect_match)
  expect_equal(nrow(res$x_only), 0)
  expect_equal(nrow(res$y_only), 0)
})

test_that("check_cohort_alignment reports asymmetric differences", {
  x <- data.frame(study = c("A", "B", "C"))
  y <- data.frame(study = c("A", "B", "D"))

  res <- check_cohort_alignment(x, y, by = "study", quiet = TRUE)

  expect_false(res$perfect_match)
  expect_equal(res$x_only$study, "C")
  expect_equal(res$y_only$study, "D")
})

test_that("check_cohort_alignment errors when keys are missing from either side", {
  x <- data.frame(study = "A")
  y <- data.frame(other = "A")

  expect_error(check_cohort_alignment(x, y, by = "study", quiet = TRUE))
})


# add_meta_pooled_results: happy path & per-group structure ----------------

test_that("add_meta_pooled_results appends one Pooled row per group", {
  skip_if_not_installed("metafor")
  df  <- make_effects_df()
  out <- add_meta_pooled_results(df, group_vars = c("outcome", "term"))

  pooled_rows <- dplyr::filter(out, cohort == "Pooled")
  expect_equal(nrow(pooled_rows), 3)

  # Cohort rows have weights; pooled rows have NA weight
  expect_true(all(!is.na(out$weight[out$cohort != "Pooled"])))
  expect_true(all(is.na(out$weight[out$cohort == "Pooled"])))
})

test_that("add_meta_pooled_results REML weights sum to ~100 within each group", {
  skip_if_not_installed("metafor")
  df  <- make_effects_df()
  out <- add_meta_pooled_results(df, group_vars = c("outcome", "term"))

  # Coerce factor cohort back to character so the filter is unambiguous,
  # then verify per-group sums explicitly.
  per_group_sums <- out %>%
    dplyr::mutate(cohort = as.character(cohort)) %>%
    dplyr::filter(cohort != "Pooled") %>%
    dplyr::group_by(outcome, term) %>%
    dplyr::summarise(total = sum(weight), .groups = "drop")

  expect_equal(nrow(per_group_sums), 3)
  expect_equal(per_group_sums$total, rep(100, 3), tolerance = 1e-6)
})

test_that("add_meta_pooled_results sums n_obs across cohorts on the pooled row", {
  skip_if_not_installed("metafor")
  df  <- make_effects_df()
  out <- add_meta_pooled_results(df, group_vars = c("outcome", "term"))

  mem_age_pool <- dplyr::filter(out, outcome == "memory", term == "ageE4",
                                cohort == "Pooled")$n_obs
  expect_equal(mem_age_pool, 500 + 400)
})


# add_meta_pooled_results: replication indicators --------------------------

test_that("add_meta_pooled_results computes same_direction and replicated for k = 2", {
  skip_if_not_installed("metafor")

  df  <- make_effects_df()
  out <- add_meta_pooled_results(df, group_vars = c("outcome", "term"))

  # memory/ageE4: both positive, |0.25/0.12| ~ 2.08 > 1.96 -> replicated
  mem_age <- dplyr::filter(out, outcome == "memory", term == "ageE4", cohort == "Pooled")
  expect_true(mem_age$same_direction)
  expect_true(mem_age$replicated)

  # memory/sex: same direction, |0.08/0.06| ~ 1.33 < 1.96 -> NOT replicated
  mem_sex <- dplyr::filter(out, outcome == "memory", term == "sex", cohort == "Pooled")
  expect_true(mem_sex$same_direction)
  expect_false(mem_sex$replicated)

  # speed/ageE4: opposite signs -> not same direction -> not replicated
  speed_age <- dplyr::filter(out, outcome == "speed", term == "ageE4", cohort == "Pooled")
  expect_false(speed_age$same_direction)
  expect_false(speed_age$replicated)
})

test_that("add_meta_pooled_results sets same_direction/replicated to NA for k >= 3", {
  skip_if_not_installed("metafor")

  df_k3 <- tibble::tribble(
    ~outcome, ~term,   ~cohort, ~estimate, ~std.error, ~n_obs,
    "y",      "x",     "C1",    0.30,      0.10,       300L,
    "y",      "x",     "C2",    0.25,      0.12,       300L,
    "y",      "x",     "C3",    0.28,      0.11,       300L
  )

  out    <- add_meta_pooled_results(df_k3, group_vars = c("outcome", "term"))
  pooled <- dplyr::filter(out, cohort == "Pooled")

  expect_equal(pooled$n_cohorts, 3)
  expect_true(is.na(pooled$same_direction))
  expect_true(is.na(pooled$replicated))
})


# add_meta_pooled_results: cohort-count edge cases -------------------------

test_that("add_meta_pooled_results errors when the input has fewer than 2 cohorts globally (Level 1)", {
  skip_if_not_installed("metafor")

  df_one_cohort <- tibble::tribble(
    ~outcome, ~term,  ~cohort,     ~estimate, ~std.error, ~n_obs,
    "y",      "x",    "Discovery", 0.30,      0.10,       300L,
    "y",      "z",    "Discovery", 0.20,      0.09,       300L
  )

  expect_error(
    add_meta_pooled_results(df_one_cohort, group_vars = c("outcome", "term")),
    regexp = "at least 2 cohorts"
  )
})

test_that("add_meta_pooled_results warns and retains lone rows for per-cell k = 1 (Level 2)", {
  skip_if_not_installed("metafor")

  # Two cohorts globally, but `attention/ageE4` is only present in Discovery
  df_unbalanced <- tibble::tribble(
    ~outcome,    ~term,   ~cohort,        ~estimate, ~std.error, ~n_obs,
    "memory",    "ageE4", "Discovery",    0.30,      0.10,       500L,
    "memory",    "ageE4", "Replication",  0.25,      0.12,       400L,
    "attention", "ageE4", "Discovery",    0.15,      0.09,       500L     # singleton cell
  )

  expect_warning(
    out <- add_meta_pooled_results(df_unbalanced, group_vars = c("outcome", "term")),
    regexp = "k = 1"
  )

  # The well-formed (memory, ageE4) group has a Pooled row;
  # the singleton (attention, ageE4) group does not.
  pooled_rows <- dplyr::filter(out, as.character(cohort) == "Pooled")
  expect_equal(nrow(pooled_rows), 1)
  expect_equal(as.character(pooled_rows$outcome), "memory")

  # The singleton row is retained, with weight = NA
  attn_rows <- dplyr::filter(out, outcome == "attention")
  expect_equal(nrow(attn_rows), 1)
  expect_true(is.na(attn_rows$weight))

  # All group_vars columns survived for the singleton row, which was the
  # original bug — group_map() strips them and the k=1 path has to re-attach.
  expect_equal(as.character(attn_rows$outcome), "attention")
  expect_equal(as.character(attn_rows$term),    "ageE4")
})


# add_meta_pooled_results: validation & customization ----------------------

test_that("add_meta_pooled_results errors when required columns are missing", {
  bad <- data.frame(outcome = "y", term = "x", cohort = "A")
  expect_error(add_meta_pooled_results(bad, group_vars = c("outcome", "term")))
})

test_that("add_meta_pooled_results respects custom pooled_label and cohort_levels", {
  skip_if_not_installed("metafor")

  df  <- make_effects_df()
  out <- add_meta_pooled_results(
    df,
    group_vars    = c("outcome", "term"),
    pooled_label  = "Meta",
    cohort_levels = c("Discovery", "Replication", "Meta")
  )

  expect_true("Meta" %in% as.character(out$cohort))
  expect_false("Pooled" %in% as.character(out$cohort))
  expect_equal(levels(out$cohort), c("Discovery", "Replication", "Meta"))
})
