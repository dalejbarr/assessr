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
                           tolerance = .002, add = TRUE) {
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
  if (!exists(obj, envir = parent.frame(), inherit = FALSE)) {
    add_feedback(paste0("* you did not define `", subvar, "`"),
                        add = add)
  } else {
    sub_val <- get(obj, envir = parent.frame(), inherits = FALSE)
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
          add_feedback("* `", subvar, "` was `NA`, `NaN`, `+Inf`, or `-Inf`", add = add)
        } else {
          res["vals_match"] <- abs(sub_val - sol_val) < tolerance
          if (res["vals_match"]) {
            add_feedback("* your values matched the solution", add = add)
          } else {
            add_feedback("* your values did not match the solution; see solution code", add = add)
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
                           tolerance = .002, add = TRUE) {
  res <- c("lengths_match" = FALSE,
           "vals_match" = FALSE)

  sol_vec <- get(solvec, envir = sol_env)
  if (!exists(subvec, envir = parent.frame(), inherit = FALSE)) {
    add_feedback(paste0("* you did not define `", subvec, "`"),
                 add = add)
  } else {
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
            res["vals_match"] <- all(abs(sub_vec - sol_vec) < tolerance)
            if (res["vals_match"]) {
              add_feedback("* correct", add = add)
            } else {
              add_feedback("* incorrect; see solution code", add = add)
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
#' @param subtbl name of the table in submission environment
#' @param sol_env the solution environment
#' @param soltbl name of the table in the solution environment
#' @param ignore.case whether to ignore case when matching submission and solution variable names
#' @param add whether to add feedback
#' @return logical; returns result of \code{dplyr::setequal(tblname, get(tblname, sol_env))}
#' @export
tbls_identical <- function(subtbl, 
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
      add_feedback("* your table `", subtbl, "` differs from the solution table; see solution code", add = add)
    } else {
      add_feedback("* your table `", subtbl, "` matched the solution table",
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
#'   \code{._ar$current_code})
#' @return logical; \code{TRUE} if the function is found anywhere in
#'   the code (comments in the code are ignored).
#' @export
code_includes <- function(fn, code) {
  any(grepl(fn_regex(fn), remove_comments(code)))
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

#' Are objects identical
#'
#' @param subobj name of submission object
#' @param sol_env solution environment
#' @param solobj name of solution object
#' @param add add feedback
#' @return logical
#' @details use this to compare any two objects (e.g., fitted model objects resulting from a call to `lm()`, `aov()`, etc)
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
#' @param add whether to add feedback
#' @return logical
#' @export
chr_vecs_equal <- function(subvar, sol_env, solvar = subvar,
                           ignore_order = FALSE,
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
        if (mode(sub_var) != "character") {
          add_feedback("* `", subvar, "` was not of type 'character'")
        } else {
          if (ignore_order) {
            res["vals_match"] <- setequal(sub_var, sol_var)
          } else {
            res["vals_match"] <- all(sub_var == sol_var)
          }
          if (res["vals_match"]) {
            add_feedback("* correct", add = add)
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
