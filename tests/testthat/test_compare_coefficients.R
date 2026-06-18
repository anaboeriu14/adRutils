test_that("compare_coefs computes the standard z-test for a difference", {
  res <- compare_coefs(b1 = 0.50, b2 = 0.30, se1 = 0.10, se2 = 0.12)

  expect_named(res, c("z_statistic", "p_value", "diff", "se_diff"))
  expect_equal(res$diff, 0.20)
  expect_equal(res$se_diff, sqrt(0.01 + 0.0144))
  expect_equal(res$z_statistic, 0.20 / sqrt(0.01 + 0.0144))
  expect_equal(res$p_value, 2 * pnorm(-abs(res$z_statistic)))
})

test_that("compare_coefs is symmetric in (b1, b2): swapping flips z but leaves p unchanged", {
  a <- compare_coefs(b1 = 0.50, b2 = 0.30, se1 = 0.10, se2 = 0.12)
  b <- compare_coefs(b1 = 0.30, b2 = 0.50, se1 = 0.12, se2 = 0.10)

  expect_equal(a$z_statistic, -b$z_statistic)
  expect_equal(a$p_value, b$p_value)
  expect_equal(a$se_diff, b$se_diff)
})


test_that("compare_coefs rejects non-positive standard errors", {
  expect_error(compare_coefs(b1 = 1, b2 = 0, se1 = 0,    se2 = 0.1))
  expect_error(compare_coefs(b1 = 1, b2 = 0, se1 = -0.1, se2 = 0.1))
})

test_that("compare_coefs rejects non-numeric or vector inputs", {
  expect_error(compare_coefs(b1 = "0.5", b2 = 0.3, se1 = 0.1, se2 = 0.1))
  expect_error(compare_coefs(b1 = c(0.5, 0.6), b2 = 0.3, se1 = 0.1, se2 = 0.1))
})
