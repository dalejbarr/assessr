code = list("two-plus-two" = "four <- 2L + 2L")
a_code = "._ar$correct <- num_vals_close(._av$result, ._as, add = FALSE)"
tvars <- c("result" = "four", "foo" = "bar")

test_that("task variables at assess_task level", {
  testenv <- new.env()
  solenv <- new.env()
  assign("four", 4L, envir = solenv)

  result <- assess_task("subx", "two-plus-two", code, a_code,
                        testenv, solenv,
                        task_varnames = tvars)$vars[[1]][["value"]]
  expect_vector(result, ptype = logical(), size = 2L)
  expect_true(all(result))
})

## create a submission
nlist_to_rmd(code, tfile <- tempfile(fileext = ".Rmd"))
nlist_to_rmd(list("two-plus-two" = a_code),
             a_file <- tempfile(fileext = ".Rmd"))

key <- compile_key(tfile, a_file, save_fig = FALSE)

test_that("task variables at assess level", {
  res <- assess(tfile, "subx", key,
                task_varnames = list("two-plus-two" = tvars))$vars[[1]]$value
  expect_vector(res, ptype = logical(), size = 2L)
  expect_true(all(res))
})

## create a directory with two submissions
subdir <- tempfile(pattern = "subs")
dir.create(subdir)
nlist_to_rmd(code, file.path(subdir, "s01.Rmd"))
nlist_to_rmd(list("two-plus-two" = "cuatro <- 2L + 2L"),
             file.path(subdir, "s02.Rmd"))
tvlist <- list("s01" = list("two-plus-two" = c("result" = "four")),
               "s02" = list("two-plus-two" = c("result" = "cuatro")))
keys <- list("s01" = compile_key(file.path(subdir, "s01.Rmd"),
                                 a_file, save_fig = FALSE),
             "s02" = compile_key(file.path(subdir, "s02.Rmd"),
                                 a_file, save_fig = FALSE))

test_that("task variables at assess_all level", {
  res <- assess_all(subdir, keys, c("s01", "s02"), task_varlist = tvlist)
  expect_equal(res$vars[[1]]$value, c(TRUE, TRUE))
  expect_equal(res$vars[[2]]$value, c(TRUE, TRUE))
})
