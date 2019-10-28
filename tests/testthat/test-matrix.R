context("matrix test values")

test_that("default correct case", {
  mx <- matrix(1:4, ncol = 2)
  e <- new.env()
  target <- rep(TRUE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- mx
  
  res <- matrix_vals_close("mx", e)

  expect_equal(target, res)
})

test_that("two of four mismatch", {
  mx <- matrix(1:4, ncol = 2, byrow = TRUE)
  e <- new.env()
  target <- c(TRUE, FALSE, FALSE, TRUE)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)

  res <- matrix_vals_close("mx", e)

  expect_equal(target, res)
})

test_that("right values, wrong dimensions", {
  mx <- matrix(1:4, nrow = 1)
  e <- new.env()
  target <- rep(TRUE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)

  res <- matrix_vals_close("mx", e)
  expect_equal(target, res)
})

test_that("wrong number of values", {
  mx <- matrix(1:6, ncol = 3)
  e <- new.env()
  target <- rep(FALSE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)
  
  res <- matrix_vals_close("mx", e)
  expect_equal(target, res)
})

test_that("not a matrix", {
  mx <- data.frame(a = 1:2, b = 3:4)
  e <- new.env()
  target <- rep(FALSE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)

  res <- matrix_vals_close("mx", e)
  expect_equal(target, res)
})

context("matrix test dimensions")

test_that("default correct case", {
  mx <- matrix(1:4, ncol = 2)
  e <- new.env()
  target <- rep(TRUE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- mx

  expect_true(matrix_same_dims("mx", e))
})

test_that("too few dimensions", {
  mx <- matrix(1:4, ncol = 1)
  e <- new.env()
  target <- rep(TRUE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)

  expect_false(matrix_same_dims("mx", e))
})

test_that("too many dimensions", {
  mx <- matrix(1:6, ncol = 3)
  e <- new.env()
  target <- rep(TRUE, 4)
  names(target) <- paste0("cell_", 1:4)
  e$mx <- matrix(1:4, ncol = 2)

  expect_false(matrix_same_dims("mx", e))
})

