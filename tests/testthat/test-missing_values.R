# summarize_na -------------------------------------------------------------

test_that("summarize_na returns one row per column with count and percent NA", {
  df <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c(NA, NA, 3, 4, 5),
    c = 1:5
  )
  res <- summarize_na(df)

  expect_named(res, c("column", "count_na", "percent_na"))
  expect_equal(nrow(res), 3)

  # Sorted descending by percent_na. colSums() returns a named numeric
  # vector and those names propagate into res$count_na, so we strip them
  # before comparing to the unnamed expected vector.
  expect_equal(as.character(res$column), c("b", "a", "c"))
  expect_equal(unname(res$count_na),   c(2, 1, 0))
  expect_equal(unname(res$percent_na), c(40, 20, 0))
})

test_that("summarize_na threshold filters out columns below the cutoff", {
  df <- data.frame(
    a = c(1, 2, NA, 4, 5),     # 20%
    b = c(NA, NA, 3, 4, 5),    # 40%
    c = 1:5                    # 0%
  )
  res <- summarize_na(df, threshold = 30)
  expect_equal(as.character(res$column), "b")
})

test_that("summarize_na rejects out-of-range threshold", {
  df <- data.frame(x = 1:3)
  expect_error(summarize_na(df, threshold = -1))
  expect_error(summarize_na(df, threshold = 150))
})


# drop_high_na_cols --------------------------------------------------------

test_that("drop_high_na_cols removes columns at or above threshold", {
  df <- data.frame(
    id            = 1:100,
    almost_empty  = c(rep(NA, 99), 1),     # 99% NA
    fully_empty   = rep(NA, 100),          # 100% NA
    mostly_filled = c(rep(NA, 20), 1:80)   # 20% NA
  )
  out <- drop_high_na_cols(df, threshold = 90, quiet = TRUE)

  expect_equal(names(out), c("id", "mostly_filled"))
})

test_that("drop_high_na_cols is a no-op when nothing crosses the threshold", {
  df <- data.frame(id = 1:10, x = c(NA, 1:9))   # 10% NA
  expect_equal(drop_high_na_cols(df, threshold = 50, quiet = TRUE), df)
})
