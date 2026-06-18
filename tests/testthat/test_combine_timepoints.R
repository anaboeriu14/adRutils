test_that("combine_timepoints picks first non-NA in priority order and labels source", {
  df <- data.frame(
    dsst30 = c(45, NA, 38),     # high-priority timepoint
    dsst35 = c(NA, 52, 41)      # fallback
  )
  out <- combine_timepoints(df, base_names = "dsst", timepoints = c("30", "35"))

  expect_equal(out$dsst_final,  c(45, 52, 38))
  expect_equal(out$dsst_source, c("30", "35", "30"))
})

test_that("combine_timepoints respects priority order when reversed", {
  df <- data.frame(
    dsst30 = c(45, NA, 38),
    dsst35 = c(50, 52, 41)
  )
  # Reverse priority: 35 wins when both present
  out <- combine_timepoints(df, base_names = "dsst", timepoints = c("35", "30"))
  expect_equal(out$dsst_final,  c(50, 52, 41))
  expect_equal(out$dsst_source, c("35", "35", "35"))
})

test_that("combine_timepoints applies custom timepoint_labels", {
  df <- data.frame(
    dsst30 = c(45, NA),
    dsst35 = c(NA, 52)
  )
  out <- combine_timepoints(
    df,
    base_names       = "dsst",
    timepoints       = c("30", "35"),
    timepoint_labels = c("30" = "Wave 1", "35" = "Wave 2")
  )
  expect_equal(out$dsst_source, c("Wave 1", "Wave 2"))
})

test_that("combine_timepoints handles multiple base_names independently", {
  df <- data.frame(
    dsst30  = c(45, NA),
    dsst35  = c(NA, 52),
    ravlt30 = c(NA, 30),
    ravlt35 = c(28, 32)
  )
  out <- combine_timepoints(df, base_names = c("dsst", "ravlt"),
                             timepoints = c("30", "35"))

  expect_equal(out$dsst_final,  c(45, 52))
  expect_equal(out$ravlt_final, c(28, 30))
  expect_equal(out$ravlt_source, c("35", "30"))
})

test_that("combine_timepoints skips base_names with no matching columns", {
  df  <- data.frame(dsst30 = c(45, NA), dsst35 = c(NA, 52))
  out <- combine_timepoints(df, base_names = c("dsst", "missing_test"),
                             timepoints = c("30", "35"))

  expect_true("dsst_final" %in% names(out))
  expect_false("missing_test_final" %in% names(out))
})

test_that("combine_timepoints errors on bad inputs", {
  df <- data.frame(x30 = 1:3, x35 = 4:6)
  expect_error(combine_timepoints(df, base_names = "x", timepoints = "30"))
  expect_error(combine_timepoints(df, base_names = character(0),
                                   timepoints = c("30", "35")))
})
