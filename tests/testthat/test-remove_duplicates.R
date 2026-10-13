# Shared fixture:
#   id 1: unique
#   id 2: differ only in missingness -> keep the complete row (age = 65)
#   id 3: identical rows             -> keep the first copy
#   id 4: conflicting age            -> warn, keep the complete row (age = 50)
make_dupes <- function() {
  data.frame(
    id  = c(1, 2, 2, 3, 3, 4, 4),
    age = c(60, NA, 65, 70, 70, 50, 55),
    bmi = c(22, 24, 24, 30, 30, 21, NA)
  )
}


# --- most_complete (default) -----------------------------------------------

test_that("most_complete keeps one row per ID", {
  result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4L)
  expect_equal(result$id, c(1, 2, 3, 4))
})

test_that("rows differing only in missingness keep the most complete row", {
  result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)

  expect_equal(result$age[result$id == 2], 65)
  expect_equal(result$bmi[result$id == 2], 24)
})

test_that("identical rows keep the first copy", {
  result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)

  # Original row 4 is the first copy of id 3
  expect_equal(rownames(result)[result$id == 3], "4")
})

test_that("conflicting rows keep the most complete row", {
  result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)

  expect_equal(result$age[result$id == 4], 50)
  expect_equal(result$bmi[result$id == 4], 21)
})

test_that("conflicting IDs are reported in a warning", {
  msgs <- capture_messages(remove_duplicates(make_dupes(), "id"))

  expect_true(any(grepl("conflicting values", msgs)))
  expect_true(any(grepl("4", msgs[grepl("conflicting values", msgs)])))
})

test_that("conflicting IDs are recorded in the attribute", {
  result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)

  expect_equal(attr(result, "conflicting_ids"), "4")
})

test_that("conflict attribute is recorded even when quiet = TRUE", {
  expect_silent(
    result <- remove_duplicates(make_dupes(), "id", quiet = TRUE)
  )
  expect_equal(attr(result, "conflicting_ids"), "4")
})

test_that("non-conflicting duplicates do not warn", {
  df <- data.frame(id = c(1, 1, 2), val = c(NA, 5, 3))

  msgs   <- capture_messages(result <- remove_duplicates(df, "id"))

  expect_false(any(grepl("conflicting values", msgs)))
  expect_length(attr(result, "conflicting_ids"), 0L)
})

test_that("conflicting rows with equal NA counts keep the first row", {
  df <- data.frame(id = c(1, 1), age = c(60, 70), bmi = c(NA, NA))

  result <- remove_duplicates(df, "id", quiet = TRUE)

  expect_equal(result$age, 60)
  expect_equal(attr(result, "conflicting_ids"), "1")
})

test_that("duplicated NA IDs are handled as one group", {
  df <- data.frame(id = c(NA, NA, 1), val = c(NA, 5, 3))

  result <- remove_duplicates(df, "id", quiet = TRUE)

  expect_equal(nrow(result), 2L)
  expect_equal(result$val[is.na(result$id)], 5)
})

test_that("character and factor IDs work", {
  df_chr <- data.frame(id = c("a", "a", "b"), val = c(NA, 1, 2))
  df_fct <- data.frame(id = factor(c("a", "a", "b")), val = c(NA, 1, 2))

  res_chr <- remove_duplicates(df_chr, "id", quiet = TRUE)
  res_fct <- remove_duplicates(df_fct, "id", quiet = TRUE)

  expect_equal(res_chr$val, c(1, 2))
  expect_equal(res_fct$val, c(1, 2))
})


# --- other strategies --------------------------------------------------------

test_that("keep = 'first' keeps the first row per ID", {
  result <- remove_duplicates(make_dupes(), "id", keep = "first", quiet = TRUE)

  expect_equal(result$age, c(60, NA, 70, 50))
})

test_that("keep = 'last' keeps the last row per ID", {
  result <- remove_duplicates(make_dupes(), "id", keep = "last", quiet = TRUE)

  expect_equal(result$age, c(60, 65, 70, 55))
})

test_that("keep = 'none' drops every row with a duplicated ID", {
  result <- remove_duplicates(make_dupes(), "id", keep = "none", quiet = TRUE)

  expect_equal(result$id, 1)
})

test_that("conflict attribute is only set for most_complete", {
  result <- remove_duplicates(make_dupes(), "id", keep = "first", quiet = TRUE)

  expect_null(attr(result, "conflicting_ids"))
})


# --- input handling and edge cases -------------------------------------------

test_that("no duplicates returns the input unchanged", {
  df <- data.frame(id = 1:3, val = c(1, 2, 3))

  result <- remove_duplicates(df, "id", quiet = TRUE)

  expect_identical(result, df)
})

test_that("unquoted id_col works", {
  result <- remove_duplicates(make_dupes(), id, quiet = TRUE)

  expect_equal(result$id, c(1, 2, 3, 4))
})

test_that("single-column input returns a data frame", {
  df <- data.frame(id = c(1, 1, 2))

  for (k in c("most_complete", "first", "last", "none")) {
    result <- remove_duplicates(df, "id", keep = k, quiet = TRUE)
    expect_s3_class(result, "data.frame")
  }
})

test_that("missing ID column errors", {
  expect_error(
    remove_duplicates(make_dupes(), "subject_id", quiet = TRUE),
    "not found"
  )
})

test_that("invalid keep errors", {
  expect_error(
    remove_duplicates(make_dupes(), "id", keep = "random", quiet = TRUE)
  )
})

test_that("quiet = TRUE suppresses all messages", {
  expect_silent(remove_duplicates(make_dupes(), "id", quiet = TRUE))
})
