# Helpers -------------------------------------------------------------------

make_outlier_df <- function() {
  data.frame(
    id         = 1:6,
    weight     = c(70, 72, 68, 200, 71, 69),
    log_weight = log(c(70, 72, 68, 200, 71, 69)),
    height     = c(170, 172, 168, 175, 171, 169)
  )
}


# detect_outlier_thresholds ------------------------------------------------

test_that("detect_outlier_thresholds returns LB, UB, notes with hand-computed bounds", {
  df  <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100))
  res <- detect_outlier_thresholds(df, "x")

  q1  <- quantile(df$x, 0.25)
  q3  <- quantile(df$x, 0.75)
  iqr <- q3 - q1

  expect_named(res, c("LB", "UB", "notes"))
  expect_equal(res$LB, unname(q1 - 1.5 * iqr))
  expect_equal(res$UB, unname(q3 + 1.5 * iqr))
  expect_equal(res$notes, "x")
})

test_that("detect_outlier_thresholds widens the band with multiplier = 3", {
  df    <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100))
  tight <- detect_outlier_thresholds(df, "x", multiplier = 1.5)
  loose <- detect_outlier_thresholds(df, "x", multiplier = 3)

  expect_lt(loose$LB, tight$LB)
  expect_gt(loose$UB, tight$UB)
})

test_that("detect_outlier_thresholds uses custom label in notes when provided", {
  df  <- data.frame(x = 1:10)
  res <- detect_outlier_thresholds(df, "x", label = "BMI (kg/m^2)")
  expect_equal(res$notes, "BMI (kg/m^2)")
})

test_that("detect_outlier_thresholds rejects bad inputs", {
  df <- data.frame(x = 1:10, y = letters[1:10])

  expect_error(detect_outlier_thresholds(df, "missing_col"))
  expect_error(detect_outlier_thresholds(df, "y"))                          # non-numeric
  expect_error(detect_outlier_thresholds(df, "x", multiplier = -1))
  expect_error(detect_outlier_thresholds(df, "x", multiplier = 0))
  expect_error(detect_outlier_thresholds(df, "x", label = c("a", "b")))     # wrong length
})


# replace_outliers_with_na -------------------------------------------------
#
# Most tests pass remove_all_na_rows = FALSE so row indices in the output
# line up with the input. The row-dropping behavior is tested separately.

test_that("replace_outliers_with_na replaces outliers in the target column with NA", {
  df  <- make_outlier_df()
  out <- replace_outliers_with_na(df, var_names = "weight",
                                  remove_all_na_rows = FALSE, quiet = TRUE)

  expect_true(is.na(out$weight[4]))                          # the 200 was the outlier
  expect_equal(out$weight[-4], df$weight[-4])                # nothing else touched
})

test_that("replace_outliers_with_na is idempotent (running twice == running once)", {
  df    <- make_outlier_df()
  once  <- replace_outliers_with_na(df,   var_names = "weight",
                                    remove_all_na_rows = FALSE, quiet = TRUE)
  twice <- replace_outliers_with_na(once, var_names = "weight",
                                    remove_all_na_rows = FALSE, quiet = TRUE)

  expect_equal(twice, once)
})

test_that("replace_outliers_with_na nullifies paired columns at outlier rows only", {
  df  <- make_outlier_df()
  out <- replace_outliers_with_na(
    df,
    var_names          = "weight",
    paired_cols        = list(weight = "log_weight"),
    remove_all_na_rows = FALSE,
    quiet              = TRUE
  )

  expect_true(is.na(out$weight[4]))
  expect_true(is.na(out$log_weight[4]))
  expect_equal(out$log_weight[-4], df$log_weight[-4])
  expect_equal(out$height, df$height)
})

test_that("replace_outliers_with_na drops rows that are all-NA across var_names by default", {
  df <- data.frame(
    a = c(10, 11, 9, 1000, 12),
    b = c(20, 22, 18, 5000, 21)
  )
  out <- replace_outliers_with_na(df, var_names = c("a", "b"), quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$a) & is.na(out$b)))
})

test_that("replace_outliers_with_na keeps all-NA rows when remove_all_na_rows = FALSE", {
  df <- data.frame(
    a = c(10, 11, 9, 1000, 12),
    b = c(20, 22, 18, 5000, 21)
  )
  out <- replace_outliers_with_na(
    df,
    var_names          = c("a", "b"),
    remove_all_na_rows = FALSE,
    quiet              = TRUE
  )

  expect_equal(nrow(out), 5)
  expect_true(is.na(out$a[4]) && is.na(out$b[4]))
})

test_that("replace_outliers_with_na skips non-numeric columns with a CLI message", {
  df <- data.frame(
    x = c(1:10, 100),
    y = letters[1:11]
  )
  # cli::cli_alert_warning emits a *message*, not an R warning(),
  # so we use expect_message rather than expect_warning.
  expect_message(
    replace_outliers_with_na(df, var_names = c("x", "y"), quiet = FALSE),
    regexp = "non-numeric"
  )
})

test_that("replace_outliers_with_na is a no-op when no outliers exist", {
  df  <- data.frame(x = c(10, 11, 12, 13, 14, 15))
  out <- replace_outliers_with_na(df, var_names = "x",
                                  remove_all_na_rows = FALSE, quiet = TRUE)
  expect_equal(out, df)
})

test_that("replace_outliers_with_na rejects bad inputs", {
  df <- make_outlier_df()
  expect_error(replace_outliers_with_na(df, var_names = character(0)))
  expect_error(replace_outliers_with_na(df, var_names = "weight",
                                        paired_cols = list("log_weight")))   # unnamed list
  expect_error(replace_outliers_with_na(df, var_names = "weight", multiplier = 0))
})
