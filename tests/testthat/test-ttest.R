test_that("default correct case", {
  set.seed(62)
  target <- c(mmatch = TRUE, tmatch = TRUE, dfmatch = TRUE, pmatch = TRUE)

  myt <- t.test(rnorm(10), rnorm(10), var.equal = TRUE)
  e <- new.env()
  e$myt <- myt

  res <- ttest_identical("myt", e)
  expect_equal(res, target)
})

## todo: Test whether student ran one-sample instead of two-sample
