test_that("default correct case", {
  a <- LETTERS
  e <- new.env()
  e$a <- LETTERS
  correct <- c("lengths_match" = TRUE, "vals_match" = TRUE, "case_match" = TRUE)
  res <- chr_vecs_equal("a", e)
  
  expect_equal(correct, res)
})

test_that("default incorrect case", {
  a <- LETTERS
  e <- new.env()
  e$a <- letters
  correct <- c("lengths_match" = TRUE, "vals_match" = TRUE, "case_match" = FALSE)
  res <- chr_vecs_equal("a", e)
  
  expect_equal(correct, res)
})

test_that("default incorrect values, correct length", {
  a <- LETTERS
  e <- new.env()
  e$a <- rep("A", 26)
  correct <- c("lengths_match" = TRUE, "vals_match" = FALSE, "case_match" = FALSE)
  res <- chr_vecs_equal("a", e)
  
  expect_equal(correct, res)
})

test_that("default incorrect length", {
  a <- LETTERS
  e <- new.env()
  e$a <- LETTERS[1:5]
  correct <- c("lengths_match" = FALSE, "vals_match" = FALSE, "case_match" = FALSE)
  res <- chr_vecs_equal("a", e)
  
  expect_equal(correct, res)
})

test_that("ignore incorrect case", {
  a <- LETTERS
  e <- new.env()
  e$a <- letters
  correct <- c("lengths_match" = TRUE, "vals_match" = TRUE)
  res <- chr_vecs_equal("a", e, ignore_case = TRUE)
  
  expect_equal(correct, res)
})
