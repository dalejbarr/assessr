#' Test whether a variable was defined statically or using code
#'
#' @param x name of the variable to check
#' @param add whether to add feedback
#' @return logical; \code{TRUE} if the code defining the variable uses any function
#' @export
defined_with_code <- function(x, code, add = TRUE) {
  res <- FALSE
  code2 <- strsplit(code, "\n")[[1]] %>% remove_comments()
  ix <- grep(paste0("^", x, "[^[:alnum:]_]"), code2, ignore.case = TRUE)
  if (length(ix)) {
    code3 <- paste(code2[ix:length(code2)], collapse = "\n")
    if (grepl("[A-Za-z]+\\s{0,1}\\(", code3)) {
      res <- TRUE
    } else {
      add_feedback("* write *code* to generate the solution; writing the answer may be wrong if underlying data changes", add = add)
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
#' @param add whether to add feedback
#' @return logical; \code{TRUE} if \code{abs(x - get(x, sol_env)) < tolerance}
#' @export
are_num_vals_close <- function(valname, sol_env, ignore.case = FALSE,
                               tolerance = .002, add = TRUE) {
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
    add_feedback(paste0("* you did not define `", valname, "`",
                        add = add))
  } else {
    sub_val <- get(obj, envir = parent.frame(), inherits = FALSE)
    compare_vals <- TRUE
    if (inherits(sub_val, "data.frame")) {
      add_feedback(paste0("* `", valname, "` should be a single value, not a table", add = add))
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
        add_feedback("* `", valname, "` was not numeric", add = add)
      } else {
        res["vals_match"] <- abs(sub_val - sol_val) < tolerance
      }
    }
  }
  res
}

#' Are submission and solution tables identical
#'
#' @param subtbl name of the table in submission environment
#' @param sol_env the solution environment
#' @param soltbl name of the table in the solution environment
#' @param ignore.case whether to ignore case when matching submission and solution variable names
#' @param add whether to add feedback
#' @return logical; returns result of \code{dplyr::setequal(tblname, get(tblname, sol_env))}
#' @export
are_tables_identical <- function(subtbl, 
                                 sol_env,
                                 soltbl = subtbl,
                                 ignore.case = FALSE,
                                 add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
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
      add_feedback("* your table `", tblname, "` differs from the solution table; see solution code", add = add)
    } else {
      add_feedback("* your table `", tblname, "` matched the solution table",
                   add = add)
      res <- TRUE
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

#' Are table dimensions the same
#'
#' @param subtbl name of table in submission environment
#' @param sol_env Solution environment
#' @param soltbl name of table in solution environment
#' @param add whether to add feedback
#' @return logical
#' @export
same_tbl_dims <- function(subtbl,
                          sol_env,
                          soltbl = subtbl, add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    if (length(dim(sub_tbl)) != length(dim(sol_tbl))) {
      add_feedback(paste0("* `", tblname, "` was not a table"), add = add)
    } else {
      res <- identical(dim(sub_tbl), dim(sol_tbl))
      if (!res) {
        add_feedback(paste0("* `", tblname, "` should have been ",
                            dim(sol_tbl)[1], "x", dim(sol_tbl)[2],
                            "; yours was ",
                            dim(sub_tbl)[1], "x", dim(sub_tbl)[1]),
                     add = add)
      }
    }
  }    
  res
}

#' Are table column names the same
#'
#' @param subtbl name of table in submission environment
#' @param sol_env Solution environment
#' @param soltbl name of table in solution environment
#' @param add whether to add feedback
#' @return logical
#' @export
same_col_names <- function(subtbl,
                           sol_env,
                           soltbl = subtbl, add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    if (!(res <- setequal(colnames(sub_tbl), colnames(sol_tbl)))) {
      add_feedback(paste0("* `", tblname, "` did not have the same column names as solution table", add = add))
    }
  }
  res
}

#' Are table column values identical
#'
#' @param subtbl name of submission table
#' @param subcol name of column in submission table
#' @param sol_env solution environment
#' @param soltbl name of table of solution environment
#' @param solcol name of column in solution table
#' @param ignore_order should the ordering of the values be ignored?
#' @param add whether to add feedback
#' @return logical
#' @export
are_columns_identical <- function(subtbl, subcol,
                                  sol_env,
                                  soltbl = subtbl,
                                  solcol = subcol,
                                  ignore_order = FALSE,
                                  add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  sub_tbl <- safe_get_table(subtbl, parent.frame(), add)
  if (!is.null(sub_tbl)) {
    if (!(subcol %in% colnames(sub_tbl))) {
      add_feedback("* column name `", subcol, "` was missing from your table `",
                   subtbl, "`", add = add)
    } else {
      if (ignore_order) {
        res <- identical(sort(sub_tbl[[subcol]]),
                         sort(sol_tbl[[solcol]]))
      } else {
        res <- identical(sub_tbl[[subcol]],
                         sol_tbl[[solcol]])
      }
      if (!res) {
        add_feedback("* values or data type for column `", subcol,
                     "` in `", subtbl, "` were incorrect",
                     add = add)
      }
    }
  }
  res
}

#' Does a submission table have required columns?
#'
#' @param subtbl name of table in submission environment
#' @param subcols names of columns that should exist
#' @param add whether to give feedback
#' @return logical
#' @export
has_columns <- function(subtbl, subcols, add = TRUE) {
  res <- FALSE
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    nvec <- setdiff(subcols, colnames(sub_tbl))
    res <- length(nvec) == 0L
    if (!res) {
      add_feedback("* your table `", subtbl,
                   "` was missing the following column(s): ",
                   paste0("`", paste(nvec, collapse = "`, `"), "`"),
                   add = add)
    }
  }
  res
}

#' @export
safe_get_table <- function(tblname, env, add = TRUE) {
  res <- NULL
  if (!exists(tblname, envir = env, inherits = FALSE)) {
    add_feedback(paste0("* you did not define `", tblname, "` (your code failed because of an error, or you renamed variables given to you)",
                        add = add))
  } else {
    res <- get(tblname, envir = env, inherits = FALSE)
    if (!inherits(res, "data.frame")) {
      add_feedback(paste0("* `", tblname, "` was not a table"), add = add)
      res <- NULL
    }
  }
  res
}
