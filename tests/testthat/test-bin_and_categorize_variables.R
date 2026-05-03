test_that("bin_and_categorize_variables splits with a single cutpoint", {
  df  <- data.frame(age = c(50, 60, 70, 80))
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(col = "age", type = "cutpoints", cutpoints = 65)),
    quiet  = TRUE
  )
  # cut() with include.lowest = TRUE, right = TRUE: <=65 vs >65
  # Default labels from .default_cutpoint_labels: "<=65", ">65"
  expect_equal(as.character(out$age_group), c("<=65", "<=65", ">65", ">65"))
})

test_that("bin_and_categorize_variables uses left-open right-closed intervals (boundary at cutpoint)", {
  df  <- data.frame(x = c(60, 65, 70))   # 65 is a cutpoint
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(col = "x", type = "cutpoints", cutpoints = 65)),
    quiet  = TRUE
  )
  # right = TRUE means the interval is (..., 65]; the value 65 falls in
  # the lower bin, not the upper one.
  expect_equal(as.character(out$x_group)[2], "<=65")
})

test_that("bin_and_categorize_variables accepts custom labels matching number of bins", {
  df  <- data.frame(bmi = c(17, 22, 27, 32))
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(
      col       = "bmi",
      type      = "cutpoints",
      cutpoints = c(18.5, 25, 30),
      labels    = c("Underweight", "Normal", "Overweight", "Obese")
    )),
    quiet  = TRUE
  )
  expect_equal(
    as.character(out$bmi_group),
    c("Underweight", "Normal", "Overweight", "Obese")
  )
})

test_that("bin_and_categorize_variables errors when label count doesn't match bin count", {
  df <- data.frame(x = 1:5)
  expect_error(
    bin_and_categorize_variables(
      df,
      groups = list(list(
        col       = "x",
        type      = "cutpoints",
        cutpoints = c(2, 4),
        labels    = c("low", "high")    # 2 cutpoints -> 3 bins, but 2 labels
      )),
      quiet  = TRUE
    ),
    regexp = "Wrong number of labels"
  )
})

test_that("bin_and_categorize_variables errors on unsorted cutpoints", {
  df <- data.frame(x = 1:10)
  expect_error(
    bin_and_categorize_variables(
      df,
      groups = list(list(col = "x", type = "cutpoints", cutpoints = c(5, 3))),
      quiet  = TRUE
    ),
    regexp = "increasing order"
  )
})

test_that("bin_and_categorize_variables maps categorical values in a single pass", {
  # Single-pass means an A->B remap won't chain into B->C even if both are
  # present in the value map.
  df  <- data.frame(code = c("A", "B", "C"))
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(
      col    = "code",
      type   = "categorical",
      values = c(A = "B", B = "C")    # if chained, A would end up "C"
    )),
    quiet  = TRUE
  )
  expect_equal(as.character(out$code_group), c("B", "C", "C"))
})

test_that("bin_and_categorize_variables passes unmapped categorical values through unchanged", {
  df  <- data.frame(sex = c("M", "F", "U"))
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(
      col    = "sex",
      type   = "categorical",
      values = c(M = "Male", F = "Female"),
      name   = "sex_label"
    )),
    quiet  = TRUE
  )
  # "U" wasn't in the map, so it stays as "U"
  expect_equal(as.character(out$sex_label), c("Male", "Female", "U"))
})

test_that("bin_and_categorize_variables applies a custom function and coerces character output to factor", {
  df  <- data.frame(score = c(45, 75, 95))
  out <- bin_and_categorize_variables(
    df,
    groups = list(list(
      col       = "score",
      type      = "custom",
      custom_fn = function(x) ifelse(x >= 90, "A", ifelse(x >= 70, "B", "C"))
    )),
    quiet  = TRUE
  )
  expect_s3_class(out$score_group, "factor")
  expect_equal(as.character(out$score_group), c("C", "B", "A"))
})

test_that("bin_and_categorize_variables errors when custom_fn returns wrong length", {
  df <- data.frame(x = 1:5)
  expect_error(
    bin_and_categorize_variables(
      df,
      groups = list(list(
        col       = "x",
        type      = "custom",
        custom_fn = function(x) x[1:2]    # truncates; wrong length
      )),
      quiet  = TRUE
    ),
    regexp = "same length"
  )
})

test_that("bin_and_categorize_variables detects duplicate output column names before mutating", {
  df <- data.frame(age = 1:5, bmi = c(20, 25, 30, 35, 40))
  expect_error(
    bin_and_categorize_variables(
      df,
      groups = list(
        list(col = "age", type = "cutpoints", cutpoints = 3, name = "dup"),
        list(col = "bmi", type = "cutpoints", cutpoints = 30, name = "dup")
      ),
      quiet  = TRUE
    ),
    regexp = "Duplicate output column"
  )
})

test_that("bin_and_categorize_variables filter_missing drops rows with NA in the new columns", {
  df  <- data.frame(x = c(1, 2, NA, 4, 5))
  out <- bin_and_categorize_variables(
    df,
    groups         = list(list(col = "x", type = "cutpoints", cutpoints = 3)),
    filter_missing = TRUE,
    quiet          = TRUE
  )
  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$x_group)))
})
