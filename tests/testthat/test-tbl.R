test_that("tbls_identical", {
  tbl1 <- data.frame(a = 1:3, b = 4:6)
  tbl2 <- tbl1[3:1,]
  tbl3 <- tbl1[, 2:1]
  tbl4 <- tbl1
  tbl5 <- cbind(tbl1, data.frame(c = 7:9))
  colnames(tbl4) <- c("A", "B")
  e <- new.env()
  assign("tbl1", tbl1, e)

  ## same row order
  expect_true(tbls_identical("tbl1", e))

  ## different row order
  expect_true(tbls_identical("tbl2", e, "tbl1"))
  expect_false(tbls_identical("tbl2", e, "tbl1", roworder_strict = TRUE))

  ## different col order
  expect_true(tbls_identical("tbl3", e, "tbl1"))
  expect_false(tbls_identical("tbl3", e, "tbl1", colorder_strict = TRUE))

  ## different case
  expect_false(tbls_identical("tbl4", e, "tbl1"))
  expect_true(tbls_identical("tbl4", e, "tbl1", ignore.case = TRUE))

  ## allow submission to have extra columns not present in target
  expect_false(tbls_identical("tbl5", e, "tbl1"))
  expect_true(tbls_identical("tbl5", e, "tbl1", allow_extracols = TRUE))
})
