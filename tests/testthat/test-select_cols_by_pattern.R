make_select_df <- function() {
  data.frame(
    id         = 1:5,
    name_first = c("John", "Jane", "Bob", "Alice", "Tom"),
    name_last  = c("Smith", "Doe", "Johnson", "Brown", "Wilson"),
    age        = c(25, 30, 35, 40, 45),
    score_math = c(85, 90, 78, 92, 88),
    score_eng  = c(76, 94, 82, 88, 79)
  )
}


test_that("select_cols_by_pattern matches columns by regex (case-insensitive by default)", {
  df  <- make_select_df()
  out <- select_cols_by_pattern(df, "name")
  expect_equal(names(out), c("name_first", "name_last"))

  # Case-insensitivity
  expect_equal(names(select_cols_by_pattern(df, "NAME")), c("name_first", "name_last"))
})

test_that("select_cols_by_pattern combines multiple patterns with OR", {
  df  <- make_select_df()
  out <- select_cols_by_pattern(df, c("id", "age"))
  expect_equal(names(out), c("id", "age"))
})

test_that("select_cols_by_pattern applies exclude after include", {
  df  <- make_select_df()
  out <- select_cols_by_pattern(df, "score", exclude = "math")
  expect_equal(names(out), "score_eng")
})

test_that("select_cols_by_pattern invert returns the complement", {
  df  <- make_select_df()
  out <- select_cols_by_pattern(df, "score", invert = TRUE)
  expect_false(any(grepl("score", names(out))))
  expect_true(all(c("id", "age", "name_first", "name_last") %in% names(out)))
})

test_that("select_cols_by_pattern returns 0-column df when no match (warn_no_match = TRUE)", {
  df <- make_select_df()
  expect_message(
    out <- select_cols_by_pattern(df, "zzz_nope"),
    regexp = "No columns matched"
  )
  expect_equal(ncol(out), 0)
})

test_that("select_cols_by_pattern aborts when no match and warn_no_match = FALSE", {
  df <- make_select_df()
  expect_error(
    select_cols_by_pattern(df, "zzz_nope", warn_no_match = FALSE),
    regexp = "No columns matched"
  )
})
