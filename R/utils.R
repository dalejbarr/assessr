#' Export named list as RMarkdown file
#'
#' @param nlist A named list, with the names corresponding to chunk names for the output file.
#' @param outfile Name of the output file.
#' @param overwrite Whether to overwrite existing file.
#' @return Path to output file, returned invisibly.
#' @export
nlist_to_rmd <- function(nlist, outfile = tempfile(), overwrite = FALSE) {
  if (!overwrite) {
    if (file.exists(outfile)) {
      stop("file '", outfile, "' exists and 'overwrite' = FALSE")
    }
  }
  cat("", file = outfile, append = FALSE) # create the file
  lines <- purrr::map(names(nlist), function(.nx) {
    cat("```{r ", .nx, "}\n", sep = "", file = outfile, append = TRUE)
    cat(nlist[[.nx]], sep = "\n", file = outfile, append = TRUE)
    cat("```\n\n", file = outfile, append = TRUE)
  })
  invisible(outfile)
}

#' Round up from .5
#'
#' @param x a numeric string (or number that can be converted to a
#'   string)
#' @param digits integer indicating the number of decimal places
#'   (`round`) or significant digits (`signif`) to be used.
#' @details Implements rounding using the "round up from .5" rule,
#'   which is more conventional than the "round to even" rule
#'   implemented by R's built-in \code{\link{round}} function. This
#'   implementation was taken from
#'   (https://stackoverflow.com/a/12688836).
#' @export
round2 = function(x, digits = 0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' Vectorized version of all.equal
#'
#' @param x vector whose elements you want to compare
#' @param y vector whose elements you want to compare to x
#' @details performs element-by-element comparison, dealing properly
#'   with floating-point values
#' @export
`%==%` <- function(x, y) {
  mapply(function(x1, y1) {
    isTRUE(all.equal(x1, y1))
  }, x, y, USE.NAMES = FALSE)
}

#' Test whether a variable was defined statically or using code
#'
#' @param x name of the variable to check
#' @param ignore_case whether to ignore case of \code{x}
#' @param add whether to add feedback
#' @return logical; \code{TRUE} if the code defining the variable uses any function
#' @export
def_by_code <- function(x, code, ignore_case = FALSE, add = TRUE) {
  res <- FALSE
  ##browser()
  code2 <- code %>% remove_comments()
  code_head <- paste0("(^|.*;)\\s*(", x, "\\s*)(<-|=)")
  ix <- grep(code_head, code2,
             ignore.case = ignore_case)
  ##stop(paste(code2, collapse = "\n"))
  if (length(ix)) {
    code3 <- paste(code2[ix[length(ix)]:length(code2)], collapse = "\n")
    ## TODO: remove anything preceding
    code4 <- as.character(parse(text = sub(code_head, "\\2\\3", code3))[1])
    ## if (grepl("[A-Za-z]+\\s{0,1}\\(", code4)) {
    if (!grepl(paste0(code_head, "\\s*[0-9]"), code4)) {
      res <- TRUE
    } else {
      add_feedback("* write *code* to generate the solution; writing a numeric answer may become wrong if underlying data changes", add = add)
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
#' @param subvar Name of the submission variable whose value you want to test
#' @param sol_env solution environment
#' @param solvar Name of the solution variable to test against
#' @param ignore.case whether to accept same variable name but different capitalization
#' @param tolerance how close the values have to be
#' @param add whether to add feedback
#' @return logical; \code{TRUE} if \code{abs(x - get(x, sol_env)) < tolerance}
#' @export
num_vals_close <- function(subvar, sol_env, solvar = subvar,
                           ignore.case = FALSE,
                           tolerance = .002, add = TRUE,
                           inherits = FALSE) {
  res <- c("is_single_val" = FALSE,
           "vals_match" = FALSE)

  sol_val <- get(solvar, envir = sol_env)
  obj <- subvar
  if (ignore.case) {
    obj <- grep(paste0("^", subvar, "$"),
                ls(parent.frame()), ignore.case = TRUE, value = TRUE)
    if (length(obj) != 1) {
      obj <- subvar # cancel
    }
  }
  if (!exists(obj, envir = parent.frame(), inherits = inherits)) {
    add_feedback(paste0("* you did not define `", subvar, "`"),
                        add = add)
  } else {
    sub_val <- safe_get_num(obj, env = parent.frame(), inherits = inherits, add = FALSE)
    ## sub_val <- get(obj, envir = parent.frame(), inherits = inherits)
    compare_vals <- TRUE
    if (inherits(sub_val, "data.frame")) {
      add_feedback(paste0("* `", subvar, "` should be a single value, not a table"), add = add)
      if (nrow(sub_val) == 1) {
        ## find the numeric columns and compare them all
          lgl_ix <- purrr::map_lgl(sub_val, ~ is.numeric(.x) | inherits(.x, "difftime"))
          if (sum(lgl_ix) > 0L) {
            vec <- as.vector(sub_val[1, lgl_ix])
            res["vals_match"] <- any((vec - sol_val) < tolerance)
            if (is.na(res["vals_match"])) {
              res["vals_match"] <- FALSE
              add_feedback("* `", subvar, "` had value `NA`")
            }
          }
      } else {
        add_feedback("* `", subvar, "` was a table containing multiple rows; should have been a vector or at least, a table with only one row", add = add)
      }
    } else if (is.vector(sub_val)) { ## it's a vector
      if (length(sub_val) == 1L) {
        res["is_single_val"] <- TRUE
      } else {
        add_feedback("* `", subvar, "` was not a single value; comparing first element to solution")
        sub_val <- sub_val[1]
      }
      if (!is.numeric(sub_val)) {
        add_feedback("* `", subvar, "` was not numeric", add = add)
      } else {
        if (is.nan(sub_val) || is.infinite(sub_val) || is.na(sub_val)) {
          add_feedback("* `", subvar, "` was `NA`, `NaN`, `+Inf`, or `-Inf`",
                       add = add)
        } else {
          res["vals_match"] <- abs(sub_val - sol_val) < tolerance
          vv <- if (length(sol_val) > 1L) "values" else "value"
          if (res["vals_match"]) {
            add_feedback("* your ", vv, " for `", subvar, "` ",
                         "matched the solution", add = add)
          } else {
            add_feedback("* your ", vv, " for `", subvar, "` ",
                         "did not match the solution; see solution code",
                         add = add)
          }
        }
      }
    } else {
      add_feedback("* `", subvar, "` was not a vector", add = add)
    }
  }
  res
}

#' Are vector element values close?
#'
#' @param subvec name of the vector in the submission environment
#' @param sol_env the solution environment
#' @param tolerance how close the values have to be
#' @param add whether to add feedback
#' @return logical
#' @export
vec_vals_close <- function(subvec, sol_env, solvec = subvec,
                           tolerance = .002,
                           add = TRUE) {
  res <- c("lengths_match" = FALSE,
           "vals_match" = FALSE)

  sol_vec <- get(solvec, envir = sol_env)
  if (!exists(subvec, envir = parent.frame(), inherits = FALSE)) {
    add_feedback(paste0("* you did not define `", subvec, "`"),
                 add = add)
  } else {
    ## variable exists
    sub_vec <- get(subvec, envir = parent.frame(), inherits = FALSE)
    if (is.vector(sub_vec)) {
      if (length(sub_vec) == length(sol_vec)) {
        res["lengths_match"] <- TRUE
        if (!is.numeric(sub_vec)) {
          add_feedback("* `", subvec, "` was not numeric", add = add)
        } else {
          if (any(is.nan(sub_vec)) || any(is.infinite(sub_vec))) {
            add_feedback("* `", subvec,
                         "` contained `NaN`, `+Inf`, or `-Inf` values",
                         add = add)
          } else {
            ## if there aren't NAs in the solution, but there are in the
            ## submission, then it is wrong
            if (!any(is.na(sol_vec)) && any(is.na(sub_vec))) {
              add_feedback("* your answer contained NA values")
            } else { 
              if (length(sub_vec[!is.na(sub_vec)]) !=
                  length(sol_vec[!is.na(sol_vec)])) {
                add_feedback("* incorrect; see solution code")
              } else {
                res["vals_match"] <- all(abs(sub_vec - sol_vec) < tolerance,
                                         na.rm = TRUE) &&
                  (length(sub_vec) > 0L)
                if (res["vals_match"]) {
                  add_feedback("* correct", add = add)
                } else {
                  add_feedback("* incorrect; see solution code", add = add)
                }
              }
            }
          }
        }
      } else {
        add_feedback("* length of `", subvec,
                     "` did not match solution; had ", length(sub_vec),
                     " elements, but should have had ", length(sol_vec),
                     add = add)
      }
    } else {
      add_feedback("* `", subvec, "` was not a vector", add = add)
    }
  }
  res  
}

#' Are submission and solution tables identical
#'
#' @param subtbl Name of the table in submission environment.
#' @param sol_env The solution environment.
#' @param soltbl Name of the table in the solution environment.
#' @param ignore.case Whether to ignore case when matching submission and solution variable names.
#' @param roworder_strict Whether to require the submission table to have rows in the same order as the solution table.
#' @param colorder_strict Whether to require the submission table to have columns in the same order as the target table.
#' @param allow_extracols Whether to allow the submission table to have extra columns not present in the solution table.
#' @param add whether to add feedback
#' @return logical; returns result of \code{dplyr::setequal(tblname, get(tblname, sol_env))}
#' @export
tbls_identical <- function(subtbl, 
                           sol_env,
                           soltbl = subtbl,
                           ignore.case = FALSE,
                           roworder_strict = FALSE,
                           colorder_strict = FALSE,
                           allow_extracols = FALSE,
                           add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    sol2 <- sol_tbl
    sub2 <- sub_tbl
    if (ignore.case) {
      ## only change if it won't produce an error
      if (dplyr::n_distinct(colnames(sub2)) == ncol(sub2)) {
        colnames(sol2) <- tolower(colnames(sol_tbl))
        colnames(sub2) <- tolower(colnames(sub_tbl))
      }
    }
    ## check for list columns, which will cause setequal to fail
    sub_types <- purrr::map_chr(sub2, class)
    sol_types <- purrr::map_chr(sol2, class)
    if (any(sub_types == "list")) {
      warning("tbls_identical is dropping list-column(s) named '",
              paste(colnames(sub2)[sub_types == "list"], collapse = ", "),
              "' from submission table '", subtbl, "'")
      sub2 <- sub2[sub_types != "list"]
    }
    if (any(sol_types == "list")) {
      warning("tbls_identical is dropping list-column(s) named '",
              paste(colnames(sol2)[sol_types == "list"], collapse = ", "),
              "' from solution table '", soltbl, "'")
      sol2 <- sol2[sol_types != "list"]      
    }
    if (allow_extracols) {
      ## reduce submission to include only cols in solution
      sub2 <- sub2[, intersect(colnames(sol2), colnames(sub2))]
    }
    if (!dplyr::setequal(sol2, sub2)) {
      add_feedback("* your table `", subtbl, "` differs from the solution table; see solution code", add = add)
    } else {
      ## strict on rows or columns?
      .testrow <- TRUE
      .testcol <- TRUE
      if (roworder_strict) {
        solrows <- sapply(sol2, order)
        subrows <- sapply(sub2, order)
        .testrow <- identical(subrows[, colnames(solrows)], solrows)
        if (!.testrow)
          add_feedback("* Rows of your table `", subtbl, "` were in a different order than the rows in the solution.", add = add)
      }
      if (colorder_strict) {
        .testcol <- identical(colnames(sol2), colnames(sub2))
        if (!.testcol)
          add_feedback("* Columns of your table `", subtbl, "` appeared in a different order than the columns in the solution.", add = add)
      }
      if (.testrow && .testcol) {
        add_feedback("* your table `", subtbl, "` matched the solution table",
                     add = add)
        
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
  if (fname == "%>%") {
    "%>%" # match the pipe
  } else {
    paste0("(^|[^[:alnum:]_])*(", fname, ")[[:space:]]*\\(")
  }
}

#' Test whether code includes a function
#'
#' Test whether the submission code includes function \code{fn}.
#'
#' @param fn function to search for
#' @param code the submission code (usually you pass the variable
#'   \code{._ar$current_code})
#' @param remove_comments do you want to remove comments before checking?
#' @return logical; \code{TRUE} if the function is found anywhere in
#'   the code (comments in the code are ignored).
#' @export
code_includes <- function(fn, code, remove_comments = TRUE) {
  any(grepl(fn_regex(fn),
            if (remove_comments) remove_comments(code) else code))
}

#' Same number of rows in table
#'
#' @param subtbl name of table in submission environment
#' @param sol_env solution environment
#' @param soltbl name of table in solution environment
#' @param add whether to add feedback
#' @return logical
#' @export
tbl_same_nrows <- function(subtbl, sol_env,
                           soltbl = subtbl,
                           add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    if (length(dim(sub_tbl)) != length(dim(sol_tbl))) {
      add_feedback(paste0("* `", subtbl, "` was not a table"), add = add)
    } else {
      res <- nrow(sub_tbl) == nrow(sol_tbl)
      if (!res) {
        add_feedback("* `", subtbl, "` had ", nrow(sub_tbl),
                     " rows; should have had ", nrow(sol_tbl), " rows.",
                     add = add)
      }
    }
  }    
  res  
}

#' Are table dimensions the same
#'
#' @param subtbl name of table in submission environment
#' @param sol_env Solution environment
#' @param soltbl name of table in solution environment
#' @param add whether to add feedback
#' @return logical
#' @export
tbl_same_dims <- function(subtbl,
                          sol_env,
                          soltbl = subtbl, add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    if (length(dim(sub_tbl)) != length(dim(sol_tbl))) {
      add_feedback(paste0("* `", subtbl, "` was not a table"), add = add)
    } else {
      res <- identical(dim(sub_tbl), dim(sol_tbl))
      if (!res) {
        add_feedback(paste0("* `", subtbl, "` should have been ",
                            dim(sol_tbl)[1], "x", dim(sol_tbl)[2],
                            "; yours was ",
                            dim(sub_tbl)[1], "x", dim(sub_tbl)[2]),
                     add = add)
      } else {
        add_feedback("* dimensions of your table matched the solution", add = add)
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
tbl_same_colnames <- function(subtbl,
                           sol_env,
                           soltbl = subtbl, add = TRUE) {
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  if (!is.null(sub_tbl <- safe_get_table(subtbl, parent.frame(), add))) {
    if (!(res <- setequal(colnames(sub_tbl), colnames(sol_tbl)))) {
      add_feedback(paste0("* `", subtbl, "` did not have the same column names as solution table"), add = add)
    }
  }
  res
}

#' Are table column values equal
#'
#' @param subtbl name of submission table
#' @param subcol name of column in submission table
#' @param sol_env solution environment
#' @param soltbl name of table of solution environment
#' @param solcol name of column in solution table
#' @param ignore_order should the ordering of the values be ignored?
#' @param add whether to add feedback
#' @details uses \code{\link{all.equal}} to deal with floating point values
#' @return logical
#' @export
tbl_cols_equal <- function(subtbl, subcol,
                           sol_env,
                           soltbl = subtbl,
                           solcol = subcol,
                           ignore_order = TRUE,
                           add = TRUE) {
  ##browser()
  res <- FALSE
  sol_tbl <- get(soltbl, envir = sol_env)
  sub_tbl <- safe_get_table(subtbl, parent.frame(), add)
  if (!is.null(sub_tbl)) {
    if (!(subcol %in% colnames(sub_tbl))) {
      add_feedback("* column name `", subcol, "` was missing from your table `",
                   subtbl, "`", add = add)
    } else {
      if (ignore_order) {
        res <- isTRUE(all.equal(sort(sub_tbl[[subcol]]),
                                sort(sol_tbl[[solcol]])))
      } else {
        res <- isTRUE(all.equal(sub_tbl[[subcol]],
                                sol_tbl[[solcol]]))
      }
      if (!res) {
        add_feedback("* values or data type for column `", subcol,
                     "` in `", subtbl, "` were incorrect",
                     add = add)
      } else {
        add_feedback("* values in column `", subcol, "` of `", subtbl,
                     "` matched corresponding values in solution table",
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
tbl_has_cols <- function(subtbl, subcols, add = TRUE) {
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
    add_feedback(paste0("* you did not define `", tblname, "` (your code failed because of an error, or you renamed variables given to you)"),
                        add = add)
  } else {
    res <- get(tblname, envir = env, inherits = FALSE)
    if (!inherits(res, "data.frame")) {
      add_feedback(paste0("* `", tblname, "` was not a table"), add = add)
      res <- NULL
    }
  }
  res
}

#' Are all column values unique?
#'
#' @param subtbl name of submission table
#' @param subcol name of submission column
#' @return logical
all_col_vals_unique <- function(subtbl, subcol, add = TRUE) {
  res <- FALSE
  d <- safe_get_table(subtbl, parent.frame(), add)
  if (!is.null(d)) {
    res <- length(unique(d[[subcol]])) == length(d[[subcol]])
  } else {
    add_feedback("* all values in column `", subcol, "` should be unique", add = add)
  }
  res
}

#' Get a printable string from error message
#'
#' @param x a condition
#' @return string with the error message
#' @export
get_err_string <- function(x) {
  f <- tempfile()
  msg <- conditionMessage(x)
  call <- conditionCall(x)
  cl <- class(x)[1L]
  if (!is.null(call)) 
    cat("<", cl, " in ", deparse(call), ": ", msg, ">\n", 
        sep = "", file = f)
  else cat("<", cl, ": ", msg, ">\n", sep = "", file = f)
  paste(readLines(f), collapse = "\n")
}

#' Are objects the same
#'
#' @param subobj name of submission object
#' @param sol_env solution environment
#' @param solobj name of solution object
#' @param add add feedback
#' @param all_equal whether to use \code{all_equal} instead of \code{identical}
#' @return logical
#' @details \code{objs_identical} uses \code{identical}; \code{objs_all_equal} usesuse this to compare any two objects (e.g., fitted model objects resulting from a call to `lm()`, `aov()`, etc)
#' @export
objs_identical <- function(subobj,
                           sol_env,
                           solobj = subobj,
                           add = TRUE,
                           all_equal = FALSE) {
  
  res <- FALSE
  sol_obj <- get(solobj, envir = sol_env)
  if (!exists(subobj, envir = parent.frame(), inherits = FALSE)) {
    add_feedback("* object `", subobj,
                 "` was not defined; check spelling/capitalization",
                 add = add)
  } else {
    sub_obj <- get(subobj, envir = parent.frame(), inherits = FALSE)
    if (!identical(class(sub_obj), class(sol_obj))) {
      add_feedback("* object `", subobj, "` was of incorrect type; was of class `",
                   paste(class(sub_obj), collapse = ", "), "` but should have been `",
                   paste(class(sol_obj), collapse = ", "), "`", add = add)
    } else {
      if (!is.null(sub_obj$model)) {
        attributes(sub_obj$model) <- NULL
        attributes(sol_obj$model) <- NULL
        attributes(sub_obj$terms) <- NULL
        attributes(sol_obj$terms) <- NULL
      }
      if (!all_equal) {
        res <- identical(sub_obj, sol_obj, ignore.environment = TRUE)
      } else {
        res <- isTRUE(all.equal(sub_obj, sol_obj, check.attributes = FALSE))
      }
      if (res) {
        add_feedback("* `", subobj, "` matched solution", add = add)
      } else {
        add_feedback("* `", subobj, "` did not match solution", add = add)
      }
    }
  }
  res
}


#' Are objects equal
#'
#' @describeIn objs_identical
#'
#' @export
objs_identical <- function(subobj,
                           sol_env,
                           solobj = subobj,
                           add = TRUE) {
  res <- FALSE
  sol_obj <- get(solobj, envir = sol_env)
  if (!exists(subobj, envir = parent.frame(), inherits = FALSE)) {
    add_feedback("* object `", subobj,
                 "` was not defined; check spelling/capitalization",
                 add = add)
  } else {
    sub_obj <- get(subobj, envir = parent.frame(), inherits = FALSE)
    if (!identical(class(sub_obj), class(sol_obj))) {
      add_feedback("* object `", subobj, "` was of incorrect type; was of class `",
                   paste(class(sub_obj), collapse = ", "), "` but should have been `",
                   paste(class(sol_obj), collapse = ", "), "`", add = add)
    } else {
      if (!is.null(sub_obj$model)) {
        attributes(sub_obj$model) <- NULL
        attributes(sol_obj$model) <- NULL
        attributes(sub_obj$terms) <- NULL
        attributes(sol_obj$terms) <- NULL
      }
      res <- identical(sub_obj, sol_obj, ignore.environment = TRUE)
      if (res) {
        add_feedback("* `", subobj, "` matched solution", add = add)
      } else {
        add_feedback("* `", subobj, "` did not match solution", add = add)
      }
    }
  }
  res
}


#' Safely try out a function defined in the submission
#'
#' @param fnname name of the function
#' @param add whether to add feedback
#' @param ... arguments to fnname
#' @return list with two values: \code{is_error} (logical) and \code{result}
fun_try <- function(fnname, ..., add = TRUE) {
  res <- list(error = TRUE, result = NULL)
  if (!exists(fnname, envir = parent.frame(), inherits = FALSE)) {
    add_feedback("* you didn't define the function `", fnname,"` (check capitalization and spelling)")
  } else {
    fres <- tryCatch(do.call(fnname, list(...)), error = function(e) e)
    if (evaluate::is.error(fres)) {
      res$error <- TRUE
      res$result <- NULL
      add_feedback("* could not run your function `", fnname, "`; it threw an error", add = TRUE)
    } else {
      res$error <- FALSE
      res$result <- fres
    }
  }
  res
}


#' Does function exist in submission environment?
#'
#' @param fnname name of function
#' @param add add feedback? (logical)
#' @return logical
fun_exists <- function(fnname, add = TRUE) {
  res <- FALSE
  if (!exists(fnname, envir = parent.frame(), inherits = FALSE)) {
    add_feedback("* `", fnname, "` does not exist (check spelling and capitalization)", add = add)
  } else {
    ff <- get(fnname, envir = parent.frame(), inherits = FALSE)
    if (!is.function(ff)) {
      add_feedback("* `", fnname, "` is not a function", add = add)
    } else {
      res = TRUE
    }
  }
  res
}

#' Are character vectors equal?
#'
#' @param subvar name of variable in submission environment
#' @param sol_env solution environment
#' @param solvar name of solution variable
#' @param ignore_order whether to ignore the ordering
#' @param ignore_case whether to ignore the case
#' @param add whether to add feedback
#' @return logical
#' @export
chr_vecs_equal <- function(subvar, sol_env, solvar = subvar,
                           ignore_order = FALSE,
                           ignore_case = FALSE,
                           add = TRUE) {
  res <- c("lengths_match" = FALSE, "vals_match" = FALSE, "case_match" = FALSE)
  if (!exists(subvar, parent.frame(), inherits = FALSE)) {
    add_feedback("* you did not define `", subvar,
                 "` (check spelling and capitalization)", add = add)
  } else {
    sub_var <- get(subvar, parent.frame(), inherits = FALSE)
    sol_var <- get(solvar, sol_env)
    if (!is.vector(sub_var)) {
      add_feedback("* `", subvar, "` was not a vector", add = add)
    } else {
      if (length(sub_var) != length(sol_var)) {
        add_feedback("* length of `", subvar, "` (", length(sub_var),
                     ") did not match solution (", length(sol_var), ")",
                     add = add)
      } else {
        res["lengths_match"] <- TRUE
        if (mode(sub_var) != "character") {
          add_feedback("* `", subvar, "` was not of type 'character'")
        } else {
          if (ignore_order) {
            res["vals_match"] <- setequal(tolower(sub_var), tolower(sol_var))
            res["case_match"] <- setequal(sub_var, sol_var)
          } else {
            res["vals_match"] <- identical(tolower(sub_var), tolower(sol_var))
            res["case_match"] <- identical(sub_var, sol_var)
          }
          if (res["vals_match"]) {
            if (ignore_case) {
              res <- res[1:2] # get ride of case_match
              add_feedback("* correct", add = add)
            } else if (res["case_match"]) {
              add_feedback("* correct", add = add)
            } else {
              add_feedback("* the case does not match (check uppercase versus lowercase characters)", add = add)
            }
          } else {
            add_feedback("* incorrect; see solution code", add = add)
          }
        }
      }
    }
  }
  res
}


#' Are logical vectors equal?
#'
#' @param subvar name of variable in submission environment
#' @param sol_env solution environment
#' @param solvar name of solution variable
#' @param add whether to add feedback
#' @return logical
#' @export
lgl_vecs_equal <- function(subvar, sol_env, solvar = subvar,
                           add = TRUE) {
  res <- c("lengths_match" = FALSE, "vals_match" = FALSE)
  if (!exists(subvar, parent.frame(), inherits = FALSE)) {
    add_feedback("* you did not define `", subvar,
                 "` (check spelling and capitalization)", add = add)
  } else {
    sub_var <- get(subvar, parent.frame(), inherits = FALSE)
    sol_var <- get(solvar, sol_env)
    if (!is.vector(sub_var)) {
      add_feedback("* `", subvar, "` was not a vector", add = add)
    } else {
      if (length(sub_var) != length(sol_var)) {
        add_feedback("* length of `", subvar, "` (", length(sub_var),
                     ") did not match solution (", length(sol_var), ")",
                     add = add)
      } else {
        res["lengths_match"] <- TRUE
        if (mode(sub_var) != "logical") {
          add_feedback("* `", subvar, "` was not of type 'logical'", add = add)
        } else {
          res["vals_match"] <- all(sub_var == sol_var)
          if (res["vals_match"]) {
            add_feedback("* `", subvar, "` matched the solution", add = add)
          } else {
            add_feedback("* `", subvar, "` did not match the solution", add = add)
          }
        }
      }
    }
  }
  res
}

#' Are lmer models identical?
#'
#' @param subvar name of submission variable
#' @param solenv solution environment
#' @param solvar name of solution variable
#' @param add whether to add feedback
#' @return logical
#' @export
lmerMods_identical <- function(subvar, solenv, solvar = subvar, add = TRUE) {
  res <- FALSE
  if (!exists(subvar, parent.frame(), inherits = FALSE)) {
    add_feedback("* `", subvar, "` was not defined", add = add)
  } else {
    submod <- get(subvar, parent.frame(), inherits = FALSE)
    if (!inherits(submod, "lmerMod")) {
      add_feedback("* `", subvar, "` was not of type `lmerMod`", add = add)
    } else {
      solmod <- get(solvar, solenv, inherits = FALSE)
      res <- all.equal(VarCorr(solmod), VarCorr(submod)) &&
        all.equal(attr(VarCorr(solmod), "sc"), attr(VarCorr(submod), "sc")) &&
        all.equal(fixef(solmod), fixef(submod))
      if (res) {
        add_feedback("* model `", subvar, "` matched solution", add = add)
      } else {
        add_feedback("* model `", subvar, "` didn't match solution", add = add)
      }
    }
  }
  res
}

#' Do return values from a function match solution?
#'
#' @param subfn name of function in submission environment
#' @param args list of args to pass to function
#' @param solenv solution environment
#' @param solfn name of solution function
#' @param sameseed whether to run the two functions with the same state of \code{.Random.seed}
#' @param add whether to add feedback
#' @details compare the result of the function in the submission environment to its result in the solution environment
#' @return logical
#' @export
fun_result_identical <- function(subfn, args = list(),
                                 solenv, solfn = subfn, sameseed = TRUE,
                                 add = TRUE) {
  result <- FALSE
  if (!exists(subfn, parent.frame(), inherits = FALSE)) {
    add_feedback("* you didn't create a function named `", subfn,
                 "` (check capitalization and spelling)", add = add)
  } else if (!is.function(get(subfn, parent.frame(), inherits = FALSE))) {
    add_feedback("* variable `", subfn, "` was not a function", add = add)
  } else {
    curseed <- get(".Random.seed", envir = globalenv())
    ## eval(set.seed(999), envir = globalenv())
    res <- try(do.call(subfn, args, envir = parent.frame()), silent = TRUE)
    if (inherits(res, "try-error")) {
      add_feedback("* your function threw an error during evaluation", add = add)
    } else {
      ## reset the seed
      if (sameseed) {
       ## assign(".Random.seed", curseed, envir = parent.frame())
       assign(".Random.seed", curseed, envir = globalenv())
      }
      ## eval(set.seed(999), envir = globalenv())
      ##eval(set.seed(999), envir = solenv)
      res2 <- do.call(solfn, args, envir = solenv)
      fres <- all.equal(res, res2)
      result <- isTRUE(fres)
      if (result) {
        add_feedback("* your function worked as expected", add = add)
      } else {
        add_feedback("* your function did not work as expected", add = add)
      }
    }
  }

  result
}

#' Are linear models identical?
#'
#' @param subvar name of variable in submission environment
#' @param solenv solution environment
#' @param solvar name of variable in solution environment
#' @param add whether to add feedback
#' @return logical value
#' @export
lms_identical <- function(subvar, solenv, solvar = subvar, add = TRUE) {
  res <- FALSE
  sol_obj <- get(solvar, envir = solenv)
  if (!exists(subvar, envir = parent.frame(), inherits = FALSE)) {
    add_feedback("* variable `", subvar,
                 "` was not defined; check spelling/capitalization",
                 add = add)
  } else {
    sub_obj <- get(subvar, envir = parent.frame(), inherits = FALSE)
    if (!identical(class(sub_obj), class(sol_obj))) {
      add_feedback("* object `", subvar, "` was of incorrect type; was of class `",
                   paste(class(sub_obj), collapse = ", "), "` but should have been `",
                   paste(class(sol_obj), collapse = ", "), "`", add = add)
    } else {
      res <- dplyr::setequal(broom::tidy(sub_obj), broom::tidy(sol_obj))
      if (res) {
        add_feedback("* `", subvar, "` matched solution", add = add)
      } else {
        add_feedback("* `", subvar, "` did not match solution", add = add)
      }
    }
  }
  res
}

#' Check whether any attempt was made to answer the question
#'
#' @param subvar Name of the submission variable to test.
#' @details If no attempt was made, then the value of \code{subvar} will remain \code{NULL}.
#' @return Returns \code{FALSE} only if \code{subvar} is \code{NULL}.
#' @export
attempted <- function(subvar, add = TRUE, inherits = FALSE) {
  res <- TRUE
  if (exists(subvar, envir = parent.frame(), inherits = inherits)) {
    sub_var <- get(subvar, envir = parent.frame(), inherits = inherits)
    res <- !is.null(sub_var)
    if (!res) {
      add_feedback("* No attempt", add = add)
    }
  } else {
    res <- FALSE
    add_feedback("* variable `", subvar, "` not defined", add = add)
  }
  res
}


#' Safely return object
#'
#' @param object name of variable
#' @param classes type of object
#' @return a list with elements \code{obj}, the object (if it exists, NULL otherwise), \code{exists} (TRUE if it does, FALSE if not), \code{type} (TRUE if object is of type 'class', otherwise FALSE), and \code{message}, a character vector describing any errors (or NULL on success)
#' @export
sget <- function(.obj, .class, env) {
  .retobj <- NULL
  .exists <- FALSE
  .type <- NA
  .message <- c("")
  if (exists(.obj, envir = env, inherits = FALSE)) {
    .retobj <- get(.obj, envir = env, inherits = FALSE)
    .exists <- TRUE
    if (inherits(.retobj, .class)) {
      .type <- TRUE
    } else {
      .retobj <- NULL
      .type <- FALSE
      .message <- paste0("* object `", .obj, "` was not of type '",
                         .class, "'")
    }
  } else {
    .message <- paste0("* object `", .obj, "` not found")
  }
  list(obj = .retobj, exists = .exists, type = .type,
       message = as.character(.message))
}

#' Are logical values the same?
#'
#' @param subvar name of variable in submission environment
#' @param sol_env solution environment
#' @param solvar name of solution variable
#' @param add whether to add feedback
#' @return logical
#' @export
lgl_vals_equal <- function(subvar, sol_env, solvar = subvar, add = TRUE) {
  result <- FALSE
  ## make sure solution variable is length 1 vector
  sol_lgl <- sget(solvar, "logical", sol_env)
  if (is.null(sol_lgl))
    stop("solution variable `", solvar, "` was not of type 'logical'")
  if (length(sol_lgl$obj) != 1L)
    stop("solution variable `", solvar, "` was not of length 1")
  sub_lgl <- sget(subvar, "logical", parent.frame())
  if (is.null(sub_lgl$obj)) {
    add_feedback(sub_lgl$message, add = add)
  } else {
    if (sol_lgl$obj == sub_lgl$obj) {
      result <- TRUE
      add_feedback("variable `", subvar, "` matched the solution", add = add)
    } else {
      add_feedback("variable `", subvar, "` did not match the solution",
                   add = add)
    }
  }
  result
}

#' Safely determine variable type
#'
#' @param subvar Name of submission variable
#' @param class Class of submission variable
#' @return logical: TRUE if 'subvar' is of type 'class'
#' @export
is_type <- function(subvar, class) {
  res <- sget(subvar, class, parent.frame())
  if (is.na(res$type)) FALSE else res$type
}

#' Test set difference
#'
#' Does the difference in elements between first set x and second set
#' y correspond to vector v?
#' 
#' @param x First set
#' @param y Second set
#' @param v Difference vector
#' @return logical
#' @export
setextra <- function(x, y, v) {
  res <- base::setdiff(x, y)
  if (length(res) == 0) {
    FALSE
  } else {
    base::setequal(res, v)
  }
}

#' Test difference in model (fixed effects) terms
#'
#' @param First model
#' @param Second model
#' @param Difference vector
#' @return logical
#' @details See whether two models differ in fixed effects terms. This can be tricky because interaction terms that are ordered differently (\code{x:y} and \code{y:x}) should be treated as identical. Any interactions will always be returned sorted in alphabetical order.
#' @export
setextra_mod <- function(x, y, v) {
  recomb <- function(.x) paste(sort(.x), collapse = ":")

  lx <- strsplit(labels(terms(x)), ":")
  ly <- strsplit(labels(terms(y)), ":")

  omitted_in_ly <- !sapply(lx, function(.x) {
    any(sapply(ly, base::setequal, .x))
  })

  lv <- strsplit(v, ":")
  lv2 <- sapply(lv, recomb)
  omt <- sapply(lx[omitted_in_ly], recomb)

  base::setequal(lv2, omt)
}

#' List all compiled html files
#'
#' List all html files compiled from RMarkdown
#'
#' @param subdir subdirectory in which to scan for html files
#' @param with_moodle_id extract the moodle identifier
#' @return if \code{with_moodle_id} is \code{FALSE}, returns a vector
#'   with paths to submission files; if \code{TRUE}, a table with
#'   submission id and filename.
#' @export
list_compiled <- function(subdir,
                          with_moodle_id = FALSE) {
  files <- list.files(sub("/$", "", subdir),
                      "\\.html$", full.names = TRUE,
                      recursive = TRUE)
  if (with_moodle_id) {
    tibble::tibble(sub_id = moodle_id(files),
                   filename = files)
  } else {
    files
  }
}

## sort interaction terms
sort_ix_terms <- function(x) {
  sapply(lapply(strsplit(x, ":"), sort), paste, collapse = ":")
}

rectify_dimnames <- function(mx) {
  names(dimnames(mx)) <- NULL
  dimnames(mx) <- lapply(dimnames(mx), as.character)
  dimnames(mx)
}

## turn lme4:::findbars() into a formula
form_fixed_match_intern <- function(subf, solf, subvar = NULL, add = FALSE) {
  res <- FALSE
  subff <- lme4:::nobars(subf)
  solff <- lme4:::nobars(solf)

  ## do RHS terms match?
  labsub <- attr(terms(subff), "term.labels")
  names(labsub) <- sort_ix_terms(labsub)
  labsub <- sort(labsub)
  labsol <- attr(terms(solff), "term.labels")
  names(labsol) <- sort_ix_terms(labsol)
  labsol <- sort(labsol)
  tmatch <- setequal(names(labsub), names(labsol))
  if (!tmatch) {
    add_feedback("* `", subvar, "` had different fixed effects terms than solution", add = add)
  } else {
    submx <- attr(terms(subff), "factors")
    solmx <- attr(terms(solff), "factors")
    if ((length(submx) == 0) || length(solmx) == 0) { # intercept only
      if (length(submx) == length(solmx)) {
        res <- TRUE
      } else {
        add_feedback("* `", subvar, "` had different fixed effects terms than solution", add = add)
      }
    } else {
      colnames(submx) <- sort_ix_terms(colnames(submx))
      colnames(solmx) <- sort_ix_terms(colnames(solmx))
      dnmatch <- all(mapply(setequal, dimnames(submx), dimnames(solmx)))
      if (dnmatch) {
        dimnames(submx) <- rectify_dimnames(submx)
        dimnames(solmx) <- rectify_dimnames(solmx)
        mxmatch <- isTRUE(all.equal(submx, solmx[dimnames(submx)[[1]],
                                                 dimnames(submx)[[2]],
                                                 drop = FALSE]))
      }
      if (!(dnmatch && mxmatch)) {
        add_feedback("* `", subvar, "` had different fixed effects structure than solution", add = add)
      } else {
        res <- TRUE
      }
    }
  }
  res
}

#' Do fixed terms in formula match?
#'
#' @param subvar Name of submission variable containing formula.
#' @param solenv Name of solution environment.
#' @param solvar Name of solution variable.
#' @param add Whether to add feedback.
#' @details Can handle regular linear model formulae (\code{Y ~ X}) as well as mixed effects models (\code{Y ~ X + (X | subject)}).
#' @return logical
#' @export
form_fixed_match <- function(subvar, solenv, solvar = subvar, 
                             add = TRUE) {
  res <- FALSE
  if (inherits(subvar, "formula")) {
    add_feedback("* `", subvar, "` was not of type 'formula'", add = add)
  } else {
    subf <- get(subvar, envir = parent.frame(), inherits = FALSE)
    solf <- get(solvar, envir = solenv)
    res <- form_fixed_match_intern(subf, solf, subvar, add)
  }
  if (res) {
    add_feedback("* fixed effects part was correct", add = add)
  }
  res
}

#' Do random terms in formula match?
#'
#' @param subvar Name of submission variable containing formula.
#' @param solenv Name of solution environment.
#' @param solvar Name of solution variable.
#' @param add Whether to add feedback.
#' @details Can handle regular linear model formulae (\code{Y ~ X}) as well as mixed effects models (\code{Y ~ X + (X | subject)}).
#' @return logical
#' @export
form_rand_match <- function(subvar, solenv, solvar = subvar, 
                             add = TRUE) {
  res <- FALSE
  if (inherits(subvar, "formula")) {
    add_feedback("* `", subvar, "` was not of type 'formula'", add = add)
  } else {
    subf <- get(subvar, envir = parent.frame(), inherits = FALSE)
    solf <- get(solvar, envir = solenv)
    if (!lme4:::anyBars(solf)) {
      stop("formula did not have random effects")
    }
    if (!lme4:::anyBars(subf)) {
      add_feedback("* `", subvar, "` did not have any random effects terms",
                   add = add)
    } else {
      ## got here; there are terms. check them
      subb <- sort(sapply(lme4:::findbars(subf), deparse))
      solb <- sort(sapply(lme4:::findbars(solf), deparse))
      if (length(subb) == length(solb)) {
        ## same length; parse out the bars
        sub_unit <- stringr::str_trim(sapply(strsplit(subb, "\\|+"), `[[`, 2L))
        sol_unit <- stringr::str_trim(sapply(strsplit(solb, "\\|+"), `[[`, 2L))
        if (any(duplicated(sub_unit))) {
          add_feedback("* same sampling unit appears more than once in random effects term", add = add)
        } else {
          if (!setequal(sub_unit, sol_unit) || (length(sub_unit) != length(sol_unit))) {
            add_feedback("* different sampling units in random effects terms from solution",
                         add = add)
          } else {
            sub_fstr <- paste0("DV ~ ", sapply(strsplit(subb, "\\|+"), `[[`, 1L))
            names(sub_fstr) <- sub_unit
            sol_fstr <- paste0("DV ~ ", sapply(strsplit(solb, "\\|+"), `[[`, 1L))
            names(sol_fstr) <- sol_unit
            sub_fstr2 <- sapply(sub_fstr, as.formula)
            sol_fstr2 <- sapply(sol_fstr, as.formula)
            res <- all(mapply(form_fixed_match_intern,
                              sub_fstr2, sol_fstr2[names(sub_fstr2)]))
            if (!res)
              add_feedback("* random effects structure did not match solution",
                           add = add)
          }
        }
      } else {
        add_feedback("* different number of random effect terms than solution",
                     add = add)
      }
    }
  }
  if (res)
    add_feedback("* random effects terms were correct", add = add)
  res
}

#' Do matrices have same dimensions?
#'
#' @param submx Quoted name of submission matrix.
#' @param sol_env Solution environment.
#' @param solmx Name of solution matrix.
#' @param add Whether to add feedback.
#' @return Logical corresponding to whether matrices have same dimensions.
#' @export
matrix_same_dims <- function(submx,
                             sol_env,
                             solmx = submx, add = TRUE) {
  res <- FALSE
  sol_mx <- get(solmx, envir = sol_env)
  sub_mx <- safe_get_type(submx, "matrix", parent.frame(), add = add)

  ## now compare
  if (!is.null(sub_mx)) {
    if (length(dim(sub_mx)) != length(dim(sol_mx))) {
      add_feedback(paste0("* `", submx, "` had the wrong number of dimensions (",
                          length(dim(sub_mx)), "); should have had ",
                          length(dim(sol_mx))), add = add)
    } else {
      res <- identical(dim(sub_mx), dim(sol_mx))
      if (!res) {
        add_feedback(paste0("* `", submx, "` should have been ",
                            dim(sol_mx)[1], "x", dim(sol_mx)[2],
                            "; yours was ",
                            dim(sub_mx)[1], "x", dim(sub_mx)[2]),
                     add = add)
      } else {
        add_feedback("* dimensions of your table matched the solution", add = add)
      }
    }
  }    
  res
}

#' Are the matrix values close?
#'
#' @param submx Quoted name of submission matrix.
#' @param sol_env Solution environment.
#' @param solmx Name of solution matrix.
#' @param tolerance How close the values need to be to be considered identical.
#' @param add Whether to add feedback.
#' @details Note that the dimensions are not checked, so can return TRUE if the values are close but come from a matrix of a different dimension.
#' @seealso \code{\link{matrix_same_dims}}
#' @return Number of cell values matching the solution.
#' @export
matrix_vals_close <- function(submx,
                              sol_env,
                              solmx = submx,
                              tolerance = .002,
                              add = TRUE) {
  sol_mx <- get(solmx, envir = sol_env)
  result <- rep(FALSE, prod(dim(sol_mx)))
  names(result) <- paste0("cell_", seq_len(prod(dim(sol_mx))))

  sub_mx <- safe_get_type(submx, "matrix", parent.frame(), add = add)

  if (!is.null(sub_mx)) {
    sol_vec <- c(sol_mx)
    sub_vec <- c(sub_mx)
    if (length(sol_vec) != length(sub_vec)) {
      add_feedback("* submission matrix had different number of values (",
                   length(sub_vec), ") from solution (",
                   length(sol_vec), ")")
    } else {
      result[] <- dplyr::near(sol_vec, sub_vec, tolerance)
      if (all(result)) {
        add_feedback("* all your matrix values matched the solution", add = add)
      } else {
        add_feedback("* ", sum(result), " of ", length(sol_vec),
                     " values in your matrix matched the solution", add = add)
      }
    }
  }
  result
}

#' Safely get a double or an integer
#' 
#' @param x Quoted name of the variable.
#' @param env The environment in which to search.
#' @param inherits Whether to search in the parent environments.
#' @param add Whether or not to add feedback.
#' @details First checks whether the variable exists in the environment. If it does, then checks whether it is of the appropriate type.
#' @return A value of the desired type if found, or \code{NULL} if not found.
#' @export
safe_get_num <- function(x, env = parent.frame(), inherits = FALSE,
                         add = TRUE) {
  ## attempt to retrieve from submission environment
  res <- NULL
  if (!exists(x, env, inherits = inherits)) {
    add_feedback(paste0("* you did not define `", x, "` (your code failed because of an error, or you renamed variables given to you)"),
                 add = add)
  } else {
    res <- get(x, envir = env, inherits = inherits)
    if (!(inherits(res, "numeric") || inherits(res, "integer"))) {
      add_feedback(paste0("* `", x, "` was not a number"),
                   add = add)
      res <- NULL
    }
  }
  res
}

#' Safely get a variable of specific type.
#'
#' @param x Quoted name of the variable.
#' @param type The data type of the variable.
#' @param env The environment in which to search.
#' @param inherits Whether to search in the parent environments.
#' @param add Whether or not to add feedback.
#' @details First checks whether the variable exists in the environment. If it does, then checks whether it is of the appropriate type.
#' @return A value of the desired type if found, or \code{NULL} if not found.
#' @export
safe_get_type <- function(x, type, env = parent.frame(), inherits = FALSE,
                          add = TRUE) {
  ## attempt to retrieve matrix from submission environment
  res <- NULL
  if (!exists(x, env, inherits = inherits)) {
    add_feedback(paste0("* you did not define `", x, "` (your code failed because of an error, or you renamed variables given to you)"),
                 add = add)
  } else {
    res <- get(x, envir = env, inherits = inherits)
    if (!inherits(res, type)) {
      add_feedback(paste0("* `", x, "` was not of type '", type, "'"),
                   add = add)
      res <- NULL
    }
  }
  res
}

#' Check whether t-test objects are identical
#'
#' @param subvar Quoted name of the variable.
#' @param sol_env Solution environment.
#' @param solvar Quoted name of variable in submission environment.
#' @param tolerance Three-element numeric vector, how close values have to be.
#' @param add Whether to add feedback.
#' @return A logical vector with elements mmatch, tmatch, dfmatch, pmatch.
#' @export
ttest_identical <- function(subvar, sol_env,
                            solvar = subvar,
                            tolerance = c(.2, .02, .2, .002), # mean, t, df, p-val
                            add = TRUE) {
  res <- c(mmatch = FALSE, tmatch = FALSE, dfmatch = FALSE, pmatch = FALSE)
  sol_t <- get(solvar, sol_env)
  sub_t <- safe_get_type(subvar, "htest", parent.frame(), add = add)

  if (!is.null(sub_t)) {
    subtbl <- broom::tidy(sub_t)
    soltbl <- broom::tidy(sol_t)

    res["mmatch"] <-
      (dplyr::near(subtbl$estimate1, soltbl$estimate1, tolerance[1]) &&
       dplyr::near(subtbl$estimate2, soltbl$estimate2, tolerance[1])) ||
      (dplyr::near(subtbl$estimate1, soltbl$estimate2, tolerance[1]) &&
       dplyr::near(subtbl$estimate2, soltbl$estimate1, tolerance[1]))
    if (is.na(res["mmatch"])) ## one-sample test
      res["mmatch"] <- FALSE
    res["tmatch"] <- dplyr::near(abs(subtbl$statistic),
                                 abs(soltbl$statistic), tolerance[2])
    res["dfmatch"] <- dplyr::near(subtbl$parameter,
                                  soltbl$parameter, tolerance[3])
    res["pmatch"] <- dplyr::near(subtbl$p.value,
                                 soltbl$p.value, tolerance[4])
    add_feedback("* solution t-test: ", apa_t(sol_t), add = add)
    add_feedback("* your t-test: ", apa_t(sub_t), add = add)
    if (all(res)) {
      add_feedback("* matched solution", add = add)
    } else {
      add_feedback("* did not match solution", add = add)
    }
  }
  res
}

#' Report a t-test in APA format
#'
#' @param x A t-test object, result of the call to \code{t.test}.
#' @return A string displaying results in APA format.
#' @export
apa_t <- function(x) {
  t_tbl <- broom::tidy(x)
  means <- format(sort(c(t_tbl$estimate1, t_tbl$estimate2)), digits = 1, nsmall = 1)
  paste0("means of ",
         paste(means, collapse = " and "), "; ",
         "$t(", round(t_tbl$parameter, 1), ") = ",
         round(abs(t_tbl$statistic), 2), "$, ",
         apa_p(t_tbl$p.value))
}

#' Report a p-value in APA format
#'
#' @param x The p-value.
#' @return A string with formatted p-value.
#' @export
apa_p <- function(x) {
  paste0("$p ",
         dplyr::case_when(x < .001 ~ "< .001",
                   x > .9994 ~ "> .999",
                   TRUE ~ sprintf("= %0.3f", x)),
         "$")
}
