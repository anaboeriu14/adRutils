test_that("convert_columns_to_factors converts matching columns to factors", {
  df <- data.frame(
    cdx_var1 = c("A", "B", "A", "C"),
    age      = c(25, 30, 22, 40),
    gender   = c("M", "M", "F", "M"),
    stringsAsFactors = FALSE
  )
  out <- convert_columns_to_factors(df, patterns = c("cdx", "gender"), quiet = TRUE)

  expect_s3_class(out$cdx_var1, "factor")
  expect_s3_class(out$gender,   "factor")
  expect_false(is.factor(out$age))
})

test_that("convert_columns_to_factors honors exclude patterns", {
  df <- data.frame(
    cdx_var1 = c("A", "B"),
    cdx_skip = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  out <- convert_columns_to_factors(df, patterns = "cdx", exclude = "skip", quiet = TRUE)

  expect_s3_class(out$cdx_var1, "factor")
  expect_false(is.factor(out$cdx_skip))
})

test_that("convert_columns_to_factors with ordered = TRUE creates ordered factors", {
  df  <- data.frame(severity = c("low", "med", "high"), stringsAsFactors = FALSE)
  out <- convert_columns_to_factors(df, patterns = "severity", ordered = TRUE, quiet = TRUE)
  expect_true(is.ordered(out$severity))
})

test_that("convert_columns_to_factors preserves unobserved levels when promoting to ordered", {
  # Promoting a regular factor to ordered should keep all levels, even
  # those not present in the data.
  df <- data.frame(
    severity = factor(c("low", "high"), levels = c("low", "med", "high")),
    stringsAsFactors = FALSE
  )
  out <- convert_columns_to_factors(df, patterns = "severity",
                                    ordered = TRUE, quiet = TRUE)

  expect_true(is.ordered(out$severity))
  expect_equal(levels(out$severity), c("low", "med", "high"))
})

test_that("convert_columns_to_factors is a no-op when nothing matches", {
  df <- data.frame(x = 1:3, y = 4:6)
  out <- convert_columns_to_factors(df, patterns = "nope", quiet = TRUE)
  expect_equal(out, df)
})
