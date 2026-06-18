make_coalesce_df <- function() {
  data.frame(
    bmi_v1 = c(22, NA, 28),
    bmi_v2 = c(NA, 25, 27),
    age_v1 = c(45, NA, 70),
    age_v2 = c(NA, 60, NA)
  )
}


test_that("coalesce_variables groups columns by pattern and takes first non-NA", {
  df  <- make_coalesce_df()
  out <- coalesce_variables(df, pattern_extract = "_v[12]", quiet = TRUE)

  expect_true(all(c("bmi", "age") %in% names(out)))
  expect_equal(out$bmi, c(22, 25, 28))
  expect_equal(out$age, c(45, 60, 70))
})

test_that("coalesce_variables manual var_groups produces named coalesced columns", {
  df  <- make_coalesce_df()
  out <- coalesce_variables(
    df,
    var_groups = list(bmi = c("bmi_v1", "bmi_v2"),
                      age = c("age_v1", "age_v2")),
    quiet      = TRUE
  )
  expect_equal(out$bmi, c(22, 25, 28))
  expect_equal(out$age, c(45, 60, 70))
})

test_that("coalesce_variables prefix is applied to new column names", {
  df  <- make_coalesce_df()
  out <- coalesce_variables(df, pattern_extract = "_v[12]",
                             prefix = "merged_", quiet = TRUE)
  expect_true(all(c("merged_bmi", "merged_age") %in% names(out)))
})

test_that("coalesce_variables refuses to overwrite by default; allows with overwrite = TRUE", {
  df       <- make_coalesce_df()
  df$bmi   <- 999                      # pre-existing collision target

  # Default: skipped, message emitted
  expect_message(
    out_skip <- coalesce_variables(df, pattern_extract = "_v[12]", quiet = FALSE),
    regexp = "Skipped existing"
  )
  expect_equal(out_skip$bmi, rep(999, 3))

  # With overwrite = TRUE: replaced
  out_over <- coalesce_variables(df, pattern_extract = "_v[12]",
                                  overwrite = TRUE, quiet = TRUE)
  expect_equal(out_over$bmi, c(22, 25, 28))
})

test_that("coalesce_variables errors when neither pattern_extract nor var_groups supplied", {
  df <- make_coalesce_df()
  expect_error(coalesce_variables(df, quiet = TRUE), regexp = "Either")
})

test_that("coalesce_variables errors when both pattern_extract and var_groups supplied", {
  df <- make_coalesce_df()
  expect_error(
    coalesce_variables(df, pattern_extract = "_v",
                        var_groups = list(bmi = c("bmi_v1", "bmi_v2")),
                        quiet = TRUE),
    regexp = "OR"
  )
})
