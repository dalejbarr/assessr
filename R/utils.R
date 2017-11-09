patch_join <- function(x, y, cols) {
  common <- intersect(names(x), names(y))
  keep_x <- setdiff(names(x), common)
  df1 <- dplyr::inner_join(dplyr::select_(x, .dots = c(cols, keep_x)),
                           dplyr::select_(y, .dots = c(cols, common)),
                           cols)
  dplyr::bind_rows(df1, dplyr::anti_join(x, df1, cols))
}

#' Visit a submission file
#'
#' @param files a vector with names of submission files
#' @param x moodle id number (see \code{\link{moodle_id}})
#' @details Finds the filename within \code{files} that corresponds to
#'   a given moodle id, and opens it with \code{file.edit}
#' @export
visit <- function(files, x) {
  file.edit(files[which(moodle_id(files) == x)])
}

#' Test whether a variable was defined statically or using code
#'
#' @param x name of the variable to check
#' @return logical; \code{TRUE} if the code defining the variable uses any function
#' @export
defined_with_code <- function(x, code) {
  res <- FALSE
  code2 <- strsplit(code, "\n")[[1]] %>% remove_comments()
  ix <- grep(paste0("^", x, "[^[:alnum:]_]"), code2, ignore.case = TRUE)
  if (length(ix)) {
    code3 <- paste(code2[ix:length(code2)], collapse = "\n")
    if (grepl("[A-Za-z]+\\s{0,1}\\(", code3)) {
      res <- TRUE
    } else {
      add_feedback("* write *code* to generate the solution; writing the answer may be wrong if underlying data changes")
    }
  }
  res
}

#' Remove comments
#'
#' @param x code
#' @return code with comments removed
#' @export
remove_comments <- function(x) {
  res <- sub("#.+$", "", x)
  res[res != ""]
}

#' Are submission and solution numeric values close
#'
#' Test whether numeric values in the submission are close to corresponding values in the solution environment.
#'
#' @param valname Name of the variable whose value you want to test
#' @param sol_env solution environment
#' @param ignore.case whether to accept same variable name but different capitalization
#' @param tolerance how close the values have to be
#' @return logical; \code{TRUE} if \code{abs(x - get(x, sol_env)) < tolerance}
#' @export
are_num_vals_close <- function(valname, sol_env, ignore.case = FALSE,
                               tolerance = .002) {
  ##browser()
  res <- c("is_single_val" = FALSE,
           "vals_match" = FALSE)

  sol_val <- get(valname, envir = sol_env)
  obj <- valname
  if (ignore.case) {
    obj <- grep(paste0("^", valname, "$"),
                ls(parent.frame()), ignore.case = TRUE, value = TRUE)
    if (length(obj) != 1) {
      obj <- valname # cancel
    }
  }
  if (!exists(obj, envir = parent.frame(), inherit = FALSE)) {
    add_feedback(paste0("* you did not define `", valname, "`"))
  } else {
    sub_val <- get(obj, envir = parent.frame(), inherits = FALSE)
    compare_vals <- TRUE
    if (inherits(sub_val, "data.frame")) {
      add_feedback(paste0("* `", valname, "` should be a single value, not a table"))
      if (nrow(sub_val) == 1) {
        ## find the numeric columns and compare them all
        lgl_ix <- purrr::map_lgl(sub_val, is.numeric)
        if (length(lgl_ix) > 0L) {
          vec <- sub_val[1, lgl_ix]
          res["vals_match"] <- any((vec - sol_val) < tolerance)
        }
      }
    } else {
      res["is_single_val"] <- TRUE
      if (!is.numeric(sub_val)) {
        add_feedback("* `", valname, "` was not numeric")
      } else {
        res["vals_match"] <- (sub_val - sol_val) < tolerance
      }
    }
  }
  res
}

#' Are submission and solution tables identical
#'
#' @param tblname name of the table
#' @param sol_env the solution environment
#' @param ignore.case whether to ignore case when matching submission and solution variable names
#' @return logical; returns result of \code{dplyr::setequal(tblname, get(tblname, sol_env))}
#' @export
are_tables_identical <- function(tblname, sol_env, ignore.case = FALSE) {
  res <- FALSE
  sol_tbl <- get(tblname, envir = sol_env)
  if (!exists(tblname, envir = parent.frame(), inherits = FALSE)) {
    add_feedback(paste0("* you did not re-define `", tblname, "` (your code failed because of an error, or you renamed variables given to you)"))
  } else {
    sub_tbl <- get(tblname, envir = parent.frame(), inherits = FALSE)
    if (!inherits(sub_tbl, "data.frame")) {
      add_feedback(paste0("* `", tblname, "` was not a table"))
    } else {
      sol2 <- sol_tbl
      sub2 <- sub_tbl
      if (ignore.case) {
        ## only change if it won't produce an error
        if (n_distinct(colnames(sub2)) == ncol(sub2)) {
          colnames(sol2) <- tolower(colnames(sol_tbl))
          colnames(sub2) <- tolower(colnames(sub_tbl))
        }
      }
      if (!dplyr::setequal(sol2, sub2)) {
        add_feedback("* your table `", tblname, "` differs from the solution table; see solution code")
      } else {
        add_feedback("* your table `", tblname, "` matched the solution table")
        res <- TRUE
      }
    }
  }
  res
}

#' Generate regular expression to search for R function call
#'
#' Generate a regular expression to use in a search function, to find a function call in R code.
#'
#' @param fname name of the function
#' @return a string containing a regular expression
#' @export
fn_regex <- function(fname) {
  paste0("(^|[^[:alnum:]_])*(", fname, ")[[:space:]]*\\(")
}

#' Test whether code includes a function
#'
#' Test whether the submission code includes function \code{fn}.
#'
#' @param fn function to search for
#' @param code the submission code (usually you pass the variable
#'   \code{current_code})
#' @return logical; \code{TRUE} if the function is found anywhere in
#'   the code (comments in the code are ignored).
#' @export
code_includes <- function(fn, code) {
  any(grepl(fn_regex(fn), remove_comments(code)))
}
