test_that("var is NA", {
  a <- suppressWarnings(mean("junk"))
  e <- new.env()
  e$a <- 99L
  correct <- c("is_single_val" = TRUE, "vals_match" = FALSE)
  res <- num_vals_close("a", e)
  
  expect_equal(correct, res)
})

test_that("var is NA_real_", {
  a <- NA_real_
  e <- new.env()
  e$a <- 99
  correct <- c("is_single_val" = TRUE, "vals_match" = FALSE)
  res <- num_vals_close("a", e)
  
  expect_equal(correct, res)
})

test_that("var is NaN", {
  a <- NaN
  e <- new.env()
  e$a <- 99
  correct <- c("is_single_val" = TRUE, "vals_match" = FALSE)
  res <- num_vals_close("a", e)
  
  expect_equal(correct, res)
})

test_that("var is +Inf", {
  a <- +Inf
  e <- new.env()
  e$a <- 99
  correct <- c("is_single_val" = TRUE, "vals_match" = FALSE)
  res <- num_vals_close("a", e)
  
  expect_equal(correct, res)
})

