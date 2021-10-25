test_that("assess single Rmd works", {
  subfile <- tempfile(fileext = ".Rmd")
  solfile <- tempfile(fileext = ".Rmd")
  afile <- tempfile(fileext = ".Rmd")

  sub <- list(T05 = "summary(\"summarydata\")\nmean_ahi <- mean(\"ahiTotal\")")
  sol <- list(T05 = "mean_ahi <- 75.3")
  acode <- list(T05 = "._ar$mean_ahi <- num_vals_close(\"mean_ahi\", ._as, tolerance = .001)")
  nlist_to_rmd(sub, subfile, TRUE)
  nlist_to_rmd(sol, solfile, TRUE)
  nlist_to_rmd(acode, afile, TRUE)

  key <- compile_key(solfile, afile, save_fig = FALSE)
  res <- assess(subfile, "testsubj", key)
  
  if (file.exists(subfile)) {
    file.remove(subfile)
  }

  if (file.exists(solfile)) {
    file.remove(solfile)
  }

  if (file.exists(afile)) {
    file.remove(afile)
  }
  
  expect_equal(res[["vars"]][[1]][["value"]],
               c(TRUE, FALSE))
})
