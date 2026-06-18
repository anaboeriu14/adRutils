test_that("compute_zscores adds prefixed columns and preserves originals", {
  df  <- data.frame(x = c(1, 2, 3, 4, 5), y = c(10, 20, 30, 40, 50))
  out <- compute_zscores(df, vars = c("x", "y"), verbose = FALSE)

  # originals untouched
  expect_identical(out$x, df$x)
  expect_identical(out$y, df$y)

  # new columns added with the default prefix
  expect_true(all(c("zscore_x", "zscore_y") %in% names(out)))
})

test_that("z-scores have mean 0 and sd 1", {
  set.seed(1)
  df  <- data.frame(x = rnorm(100, mean = 50, sd = 7))
  out <- compute_zscores(df, vars = "x", verbose = FALSE)

  expect_equal(mean(out$zscore_x), 0,  tolerance = 1e-8)
  expect_equal(sd(out$zscore_x),   1,  tolerance = 1e-8)
})

test_that("prefix argument is respected", {
  df  <- data.frame(x = c(1, 2, 3))
  out <- compute_zscores(df, vars = "x", prefix = "z_", verbose = FALSE)

  expect_true("z_x" %in% names(out))
  expect_false("zscore_x" %in% names(out))
})

test_that("group_vars produces group-wise standardization", {
  df <- data.frame(
    grp = rep(c("a", "b"), each = 5),
    x   = c(1, 2, 3, 4, 5, 101, 102, 103, 104, 105)
  )
  out <- compute_zscores(df, vars = "x", group_vars = "grp", verbose = FALSE)

  by_group <- tapply(out$zscore_x, out$grp, mean)
  expect_equal(as.numeric(by_group), c(0, 0), tolerance = 1e-8)   # was unname(by_group)
})

test_that("NA values pass through without contaminating other rows", {
  df  <- data.frame(x = c(1, 2, NA, 4, 5))
  out <- compute_zscores(df, vars = "x", verbose = FALSE)

  # the NA stays NA in the same position
  expect_true(is.na(out$zscore_x[3]))
  expect_equal(sum(is.na(out$zscore_x)), 1L)

  # observed rows are standardized using observed values only
  observed <- out$zscore_x[!is.na(out$zscore_x)]
  expect_equal(mean(observed), 0, tolerance = 1e-8)
  expect_equal(sd(observed),   1, tolerance = 1e-8)
})

test_that("invalid inputs are rejected by validate_args", {
  df <- data.frame(x = c(1, 2, 3), label = c("a", "b", "c"))

  # non-numeric column requested for standardization
  expect_error(compute_zscores(df, vars = "label", verbose = FALSE))

  # column that does not exist
  expect_error(compute_zscores(df, vars = "missing", verbose = FALSE))

  # group_vars of the wrong type
  expect_error(
    compute_zscores(df, vars = "x", group_vars = 1, verbose = FALSE)
  )
})
