test_that("summarize_na counts blanks when na_strings is supplied", {
  df <- data.frame(
    a = c(1, 2, NA, 4),
    b = c("x", "", "  ", NA),
    stringsAsFactors = FALSE
  )

  # default path unchanged: blanks NOT counted
  base <- summarize_na(df)
  expect_equal(base$count_na[base$column == "b"], 1)
  expect_false("count_missing" %in% names(base))

  # opt-in path: "" and whitespace-only count as blank, NA stays separate
  out <- summarize_na(df, na_strings = "")
  brow <- out[out$column == "b", ]
  expect_equal(brow$count_na, 1)       # the NA
  expect_equal(brow$count_blank, 2)    # "" and "  "
  expect_equal(brow$count_missing, 3)
})
