figdir <- function(sub_id, trailing = FALSE) {
  fdir <- file.path("submission_figs", sub_id)
}

#' Get blacklisted functions
#'
#' @export
get_blacklist <- function() {
  blacklist <- c("install.packages", "update.packages", "download.packages",
                 "remove.packages",
                 "tidyverse_update",
                 "setwd", "help", "vignette", "download.file", "system",
                 "help.start",
                 "file.remove", "curl", "View", "browseURL")
  other <- tibble::tribble(~fn,        ~regex,
          "? (help)", "(^|[^[:alnum:]_])*(\\?)\\s*[[:alnum:]]+",
          "library() with no arguments",
                 "(^|[^[:alnum:]_])*(library)[[:space:]]*\\(\\s*\\)")

  dplyr::bind_rows(tibble::tibble(fn = blacklist,
                                  regex = fn_regex(blacklist)),
                   other)
}

#' Add feedback to end of code block
#'
#' @param ... character strings giving feedback
#' @param sep separator for character strings
#' @param add whether to add or ignore
#' @export
add_feedback <- function(..., sep = "", add = TRUE) {
  str <- do.call("paste", c(..., list(sep = sep)))
  if (add) 
    cat(str, "\n", file = file.path(tempdir(), ".fbk"), append = TRUE)
}

#' Output the feedback
#'
#' @export
give_feedback <- function() {
  if (file.exists(file.path(tempdir(), ".fbk"))) {
    dat <- paste(stringr::str_trim(readLines(file.path(tempdir(), ".fbk"))),
                 collapse = "\n")
    reset_feedback()
    stringr::str_trim(dat)
  } else {
    invisible(NULL)
  }
}

#' Reset feedback
#'
#' @export
reset_feedback <- function() {
  if (file.exists(file.path(tempdir(), ".fbk"))) {
    file.remove(file.path(tempdir(), ".fbk"))
  }
}

#' Assess a submission
#' 
#' @param filename name of submission file
#' @param key key table (see \code{\link{compile_key}})
#' @param sub_id unique submission identifier
#' @param use_sub_env whether each submission code chunk is to be run
#'   within the submission environment (to allow for dependencies
#'   between chunks) or whether to be run in the solution environment
#'   of the previous chunk (to allow recovery from errors).  In the
#'   former case, all but the first of the `key$start_env`
#'   environments will be ignored.
#' @param workdir the working directory in which to evaluate the code
#' @param seed starting seed for random number generation (or NULL to not set the seed)
#' @return A table
#' @export
assess <- function(filename, sub_id = filename, key,
                   use_sub_env = TRUE, workdir = NULL, seed = NULL) {
  search_pre <- search()
  
  sub_chunks <- tangle(filename)
  ix <- seq_along(key[["task"]])
  names(ix) <- key[["task"]]

  this_env <- NULL
  if (use_sub_env) {
    ## use a copy so that things are fresh for each submission
    this_env <- new.env(parent = key[["start_env"]][[1]])
  }

  oldwd <- getwd()
  if (!is.null(workdir)) {
    setwd(workdir)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  res <- purrr::map(key[["task"]], function(x) {
    reset_feedback()
    if (!use_sub_env) {
      ## use a copy so that things are fresh for each submission
      this_env <- new.env(parent = key[["start_env"]][[x]])
    } else {
      #########################################
      ## TODO: run any intervening blocks here
      code_cur <- which(names(sub_chunks) == x)
      if (length(code_cur)) {  ## it exists
        ## what are the names of sub_chunks previous to this one
        chk_names_pre <- names(sub_chunks)[seq_len(code_cur - 1L)]
        if (length(chk_names_pre)) { # there are previous chunks
          ## what are the names of tasks previous to this one
          task_names_pre <- key[["task"]][seq_len(which(key[["task"]] == x) - 1L)]
          if (length(task_names_pre)) {
            ## which preceding blocks have task names
            pre <- purrr::map_lgl(chk_names_pre, ~ .x %in% task_names_pre)
            ## find the numeric index of last task that was executed
            chk_ix_pre <- max(seq_along(pre)[pre])
          } else {
            chk_ix_pre <- 0L
          }
          ## get indices of intervening blocks
          chk_todo <- setdiff(seq(chk_ix_pre, code_cur, 1L),
                              c(chk_ix_pre, code_cur))
          purrr::walk(chk_todo, function(nx) {
            imgfile <- tempfile()
            png(imgfile)
            result <- evaluate::evaluate(sub_chunks[[nx]],
                                         this_env,
                                         new_device = FALSE)
            dev.off()
            ## check for errors
            errs <- purrr::map_lgl(result, evaluate::is.error)
            if (any(errs)) {
              fbk <- paste0("your code chunk named `", names(sub_chunks)[nx],
                            "` generated the following error(s):\n```\n",
                            paste(purrr::map_chr(result[errs],
                                                 purrr::pluck, "message"),
                                  collapse = "\n"), "\n```\n")
              add_feedback(fbk)
            }
          })
        }
      }
    } ##############################################
    ## now any intervening blocks will have run

    ## create the list to contain assessment params
    ## assign("._ar", list(), envir = this_env)
    ## copy the solution environment over to this_env
    ## assign("._as", key[["sol_env"]][[x]], envir = this_env)

    ff <- list()
    ## ff$result <- assess_task(sub_id, x, sub_chunks,
    ##                         key[["a_code"]][[x]], this_env, use_sub_env)
    ## ff$error <- NULL
                      
    ff <- safely_assess_task(sub_id, x, sub_chunks,
                             key[["a_code"]][[x]],
                             this_env,
                             key[["sol_env"]][[x]],
                             use_sub_env)
    if (!is.null(ff[["error"]])) {
      setwd(oldwd)
      stop(ff[["error"]][["message"]])
    } else {
      ff[["result"]]
    }
  })
  setwd(oldwd)
  names(res) <- key[["task"]]

  restore_search_path(search_pre)
  
  tibble::enframe(res, "task") %>%
    tidyr::unnest() %>%
      dplyr::mutate(sub_id = sub_id, filename = filename,
                    code = sub_chunks[key[["task"]]]) %>%
      dplyr::select(sub_id, task, vars:fbk, err, code, filename) %>%
      dplyr::mutate(fbk = purrr::map_chr(fbk, function(x) {
        paste(unique(strsplit(x, "\n")[[1]]), collapse = "\n")
      }))
}

#' Assess individual task
#'
#' Assess an individual task from a submission
#' @param sub_id submission id
#' @param task name of task to assess
#' @param codelist list of all submission code chunks
#' @param a_code assessment code
#' @param orig_env starting environment in which to evaluate submission code
#' @param use_sub_env set to \code{TRUE} if you want to run in a single submission environment; \code{FALSE} to use the solution as starting environment
#' @export
assess_task <- function(sub_id, task, codelist, a_code,
                        orig_env, sol_env, use_sub_env = TRUE) {
  sub_code <- codelist[[task]]
  if (is.null(sub_code)) {
    sub_code <- ""
    add_feedback("code block `", task, "` was missing from your RMarkdown file",
                 sep = " ")
  }
  
  if (!use_sub_env) {
    sub_env <- new.env(parent = orig_env)
  } else {
    sub_env <- orig_env
  }
  assign("._ar", list(), sub_env)
  assign("._ac", sub_code, sub_env)
  assign("._as", sol_env, sub_env)
  fig <- ""

  ## need add_feedback() in the environment
  evaluate::evaluate(deparse(add_feedback), sub_env, new_device = FALSE)

  ## were any "forbidden" functions called?
  ## sub_code <- strsplit(sub_code, "\n")[[1]]
  forbidden <- get_blacklist()
  fb_regx <- forbidden[["regex"]]
  is_forbid <- purrr::map_lgl(fb_regx, ~ any(grepl(.x,
                                                   remove_comments(sub_code))))
  forbid <- FALSE
  if (any(is_forbid)) {
    forbid <- TRUE
    add_feedback(
      "Certain functions should *never* be called from an RMarkdown script.",
      "These functions are unsafe, require user input, or are just unnecessary.",
      "We found the following 'forbidden' function(s) in your RMarkdown script: ",
      paste(paste0("`", forbidden[is_forbid, "fn"], "()`"), collapse = ", ",
            "\n", sep = ""))
    ## remove them
    for (i in forbidden[is_forbid, "regex"]) {
      sub_code <- gsub(i, "## \\2(", sub_code)
    }
  }

  ## evaluate the submission code, saving any plots if necessary
  sub_code <- paste(sub_code, collapse = "\n")
  imgfile <- tempfile()
  png(imgfile)
  result <- evaluate::evaluate(sub_code, sub_env, new_device = FALSE)
  dev.off()
  if (file.exists(imgfile)) {
    ##
    ## if (!dir.exists(figdir(sub_id))) {
    ##   dir.create(figdir(sub_id), recursive = TRUE)
    ## }
    ## figpath <- file.path(figdir(sub_id), paste(task, "png", sep = "."))
    ## file.copy(imgfile, figpath)
    ## paste0("data:image/png;base64,", f)
    fig = paste0("data:image/png;base64,", base64enc::base64encode(imgfile))
  }

  ## check for errors
  errs <- purrr::map_lgl(result, evaluate::is.error)
  err <- FALSE
  if (any(errs)) {
    fbk <- paste0("your code generated the following error(s):\n```\n",
                  paste(purrr::map_chr(result[errs], purrr::pluck, "message"),
                        collapse = "\n"), "\n```\n")
    add_feedback(fbk)
    err <- TRUE
  }

  ## run the assessment code
  eval_res <- evaluate::evaluate(a_code, sub_env, new_device = FALSE,
                     stop_on_error = 1L)

  if ((ix <- purrr::detect_index(eval_res, evaluate::is.error)) != 0L) {
    ## TODO: fix this hacky code
    msg <- ifelse(!is.null(eval_res[[ix]][["message"]]),
                  eval_res[[ix]][["message"]], # simpleError
                  eval_res[[ix]][[1]][["message"]]) # error
    stop("assessment code failed for ", sub_id, " task ", task, ":\n", msg)
  }

  a_vars <- tibble::enframe(unlist(get("._ar", sub_env)), "var", "value")

  tibble::tibble(vars = list(a_vars),
                 fig = fig,
                 forbid = forbid,
                 fbk = stringr::str_trim(paste("", give_feedback(),
                                               collapse = "\n")),
                 err = err)
}

safely_assess_task <- purrr::safely(assess_task)

#' Create an assessment code file
#'
#' Create an assessment code file based on solution
#' 
#' @param s_file name of file with solutions
#' @return name of the output file
assessment_code <- function(s_file, o_file = "assess_code.Rmd",
                            overwrite = FALSE) {
  if (file.exists(o_file) && !overwrite) {
    stop("file '", o_file, "' exists and overwrite = FALSE")
  }

  s_code <- tangle(s_file)

  ## create the file
  append <- FALSE

  purrr::walk(names(s_code), function(x) {
    if (x == names(s_code)[[1]]) append <- FALSE else append <- TRUE
    cat(rmd_chunk_stub(x, trailing = ""),
        sep = "\n", file = o_file, append = append)
  })

  message("wrote ", length(s_code), " code chunk",
          if (length(s_code) > 1) "s" else "",
          " to file '", o_file, "'")

  return(invisible(o_file))
}
