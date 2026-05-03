# Tests for validate_args.R
#
# Most tests wrap calls inside small helper functions, because validate_args
# inspects the caller's frame to look up argument values. Calling it
# directly from the top level wouldn't exercise the realistic code path.


# Happy path ---------------------------------------------------------------

test_that("validate_args returns invisibly TRUE when all checks pass", {
  fake_fn <- function(x, y) {
    validate_args(x = is_string(), y = is_flag())
  }
  expect_true(fake_fn("hello", TRUE))
})

test_that("validate_args reports all failures in a single error", {
  fake_fn <- function(x, y) {
    validate_args(x = is_string(), y = is_flag())
  }
  err <- tryCatch(fake_fn(123, "not a flag"), error = function(e) e)

  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "x")          # complains about both
  expect_match(msg, "y")
})


# Assertion helpers --------------------------------------------------------

test_that("is_flag accepts TRUE/FALSE and rejects everything else", {
  fake_fn <- function(q) validate_args(q = is_flag())

  expect_true(fake_fn(TRUE))
  expect_true(fake_fn(FALSE))
  expect_error(fake_fn(NA))
  expect_error(fake_fn(1))
  expect_error(fake_fn(c(TRUE, FALSE)))
  expect_error(fake_fn("yes"))
})

test_that("is_string requires a single non-empty string", {
  fake_fn <- function(my_str) validate_args(my_str = is_string())

  expect_true(fake_fn("ok"))
  expect_error(fake_fn(""))
  expect_error(fake_fn(NA_character_))
  expect_error(fake_fn(c("a", "b")))
  expect_error(fake_fn(123))
})

test_that("is_nonempty_character requires a non-empty character vector", {
  fake_fn <- function(v) validate_args(v = is_nonempty_character())

  expect_true(fake_fn("a"))
  expect_true(fake_fn(c("a", "b")))
  expect_error(fake_fn(character(0)))
  expect_error(fake_fn(1:3))
})

test_that("is_number enforces single value, optional positivity, and bounds", {
  fake_fn       <- function(n) validate_args(n = is_number())
  fake_pos_fn   <- function(n) validate_args(n = is_number(positive = TRUE))
  fake_range_fn <- function(n) validate_args(n = is_number(min = 0, max = 1))

  expect_true(fake_fn(3.14))
  expect_error(fake_fn(c(1, 2)))
  expect_error(fake_fn(NA_real_))

  expect_true(fake_pos_fn(0.5))
  expect_error(fake_pos_fn(0))
  expect_error(fake_pos_fn(-1))

  expect_true(fake_range_fn(0.5))
  expect_error(fake_range_fn(-0.1))
  expect_error(fake_range_fn(1.5))
})

test_that("is_one_of restricts to allowed values", {
  fake_fn <- function(m) validate_args(m = is_one_of(c("a", "b", "c")))

  expect_true(fake_fn("a"))
  expect_true(fake_fn("c"))
  expect_error(fake_fn("d"))
  expect_error(fake_fn(c("a", "b")))
})


# Data-frame and column shorthand ------------------------------------------

test_that("validate_args' columns shorthand catches missing columns", {
  fake_fn <- function(dataf, cols) {
    validate_args(data = dataf, columns = cols)
  }
  df <- data.frame(a = 1:3, b = 4:6)

  expect_true(fake_fn(df, c("a", "b")))
  expect_error(fake_fn(df, c("a", "missing")), regexp = "not found")
})

test_that("validate_args' numeric_columns shorthand catches non-numeric columns", {
  fake_fn <- function(dataf, cols) {
    validate_args(data = dataf, numeric_columns = cols)
  }
  df <- data.frame(a = 1:3, b = letters[1:3])

  expect_true(fake_fn(df, "a"))
  expect_error(fake_fn(df, "b"), regexp = "not numeric")
})

test_that("validate_args rejects non-data-frame data argument", {
  fake_fn <- function(dataf) validate_args(data = dataf)
  expect_error(fake_fn(list(a = 1)), regexp = "data frame")
})


# Custom checks ------------------------------------------------------------

test_that("validate_args runs custom_checks and reports the supplied message", {
  fake_fn <- function(threshold) {
    validate_args(
      custom_checks = list(
        list(
          condition = is.numeric(threshold) && threshold >= 0 && threshold <= 1,
          message   = "{.arg threshold} must be in [0, 1]"
        )
      )
    )
  }
  expect_true(fake_fn(0.5))
  expect_error(fake_fn(2), regexp = "threshold")
})


# Misuse -------------------------------------------------------------------

test_that("validate_args errors when an assertion is given a bare value (not a helper)", {
  fake_fn <- function(x) validate_args(x = TRUE)
  expect_error(fake_fn("hello"), regexp = "assertion helper")
})

test_that("validate_args errors when an assertion references a nonexistent argument", {
  fake_fn <- function(x) validate_args(y = is_string())
  expect_error(fake_fn("hello"), regexp = "no such argument")
})
