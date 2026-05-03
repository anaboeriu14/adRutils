make_dupe_df <- function() {
  data.frame(
    id  = c(1, 2, 2, 3, 4, 4),
    val = c(10, NA, 20, 30, 40, NA)
  )
}


test_that("remove_duplicates with keep = 'most_complete' keeps the row with fewest NAs", {
  df  <- make_dupe_df()
  out <- remove_duplicates(df, id_col = id, keep = "most_complete", quiet = TRUE)

  # For id = 2: keep val = 20 (not NA). For id = 4: keep val = 40 (not NA).
  expect_equal(nrow(out), 4)
  expect_equal(out$val[out$id == 2], 20)
  expect_equal(out$val[out$id == 4], 40)
})

test_that("remove_duplicates with keep = 'first' keeps the first occurrence", {
  df  <- make_dupe_df()
  out <- remove_duplicates(df, id_col = id, keep = "first", quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(out$val[out$id == 2], NA_real_)    # the first id=2 row had NA
  expect_equal(out$val[out$id == 4], 40)
})

test_that("remove_duplicates with keep = 'last' keeps the last occurrence", {
  df  <- make_dupe_df()
  out <- remove_duplicates(df, id_col = id, keep = "last", quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(out$val[out$id == 2], 20)
  expect_equal(out$val[out$id == 4], NA_real_)
})

test_that("remove_duplicates with keep = 'none' drops every row with a duplicated ID", {
  df  <- make_dupe_df()
  out <- remove_duplicates(df, id_col = id, keep = "none", quiet = TRUE)

  expect_equal(out$id, c(1, 3))    # only the unique ids remain
})

test_that("remove_duplicates is a no-op when there are no duplicates", {
  df  <- data.frame(id = 1:5, val = letters[1:5])
  out <- remove_duplicates(df, id_col = id, quiet = TRUE)
  expect_equal(out, df)
})

test_that("remove_duplicates accepts both quoted and unquoted id_col", {
  df       <- make_dupe_df()
  out_unq  <- remove_duplicates(df, id_col = id,    quiet = TRUE)
  out_quot <- remove_duplicates(df, id_col = "id",  quiet = TRUE)
  expect_equal(out_unq, out_quot)
})

test_that("remove_duplicates errors when id_col doesn't exist", {
  df <- data.frame(id = 1:3)
  expect_error(remove_duplicates(df, id_col = nope, quiet = TRUE))
})
