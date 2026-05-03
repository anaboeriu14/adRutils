test_that("transform_log10 adds log10_<var> columns and preserves originals", {
  df  <- data.frame(x = c(10, 100, 1000), y = c(1, 2, 3))
  out <- transform_log10(df, vars = c("x", "y"), quiet = TRUE)

  expect_true(all(c("log10_x", "log10_y") %in% names(out)))
  expect_equal(out$log10_x, c(1, 2, 3))
  expect_equal(out$log10_y, log10(c(1, 2, 3)))
  # Originals untouched
  expect_equal(out$x, df$x)
  expect_equal(out$y, df$y)
})

test_that("transform_log10 errors on non-positive values", {
  df_zero <- data.frame(x = c(1, 0, 10))
  df_neg  <- data.frame(x = c(1, -5, 10))

  expect_error(transform_log10(df_zero, vars = "x"), regexp = "non-positive")
  expect_error(transform_log10(df_neg,  vars = "x"), regexp = "non-positive")
})

test_that("transform_log10 refuses to overwrite existing log10_ columns by default", {
  df <- data.frame(x = c(10, 100), log10_x = c(99, 99))
  expect_error(transform_log10(df, vars = "x"), regexp = "already exist")
})

test_that("transform_log10 overwrites when overwrite = TRUE", {
  df <- data.frame(x = c(10, 100), log10_x = c(99, 99))
  expect_warning(
    out <- transform_log10(df, vars = "x", overwrite = TRUE, quiet = FALSE),
    regexp = "Overwrote"
  )
  expect_equal(out$log10_x, c(1, 2))
})

test_that("transform_log10 warns on already-log10-prefixed inputs", {
  df <- data.frame(log10_x = c(1, 2, 3))
  expect_warning(
    transform_log10(df, vars = "log10_x", quiet = FALSE),
    regexp = "already log10-prefixed"
  )
})
