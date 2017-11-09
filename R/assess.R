figdir <- function(sub_id, trailing = FALSE) {
  fdir <- file.path("submission_figs", sub_id)
}

get_blacklist <- function() {
  blacklist <- c("install.packages", "update.packages", "download.packages",
                 "remove.packages",
                 "tidyverse_update",
                 "setwd", "help", "vignette", "download.file", "system",
                 "help.start",
                 "file.remove", "curl", "View", "browseURL")
  other <- tribble(~fn,        ~regex,
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
#' @export
add_feedback <- function(..., sep = " ") {
  do.call("cat", c(list(...), list("\n",
                                   file = file.path(tempdir(), ".fbk"),
                                   sep = sep, append = TRUE)))
}

#' Output the feedback
#'
#' @export
give_feedback <- function() {
  if (file.exists(file.path(tempdir(), ".fbk"))) {
    dat <- paste(readLines(file.path(tempdir(), ".fbk")), collapse = "\n\n")
    reset_feedback()
    dat
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
#' @return A table
#' @export
assess_submission <- function(filename, sub_id = filename, key,
                              use_sub_env = TRUE) {
  sub_chunks <- tangle(filename)
  ix <- seq_along(key[["task"]])
  names(ix) <- key[["task"]]

  this_env <- NULL
  if (use_sub_env) {
    this_env <- key[["start_env"]][[1]]
  }
  
  res <- purrr::map(key[["task"]], function(x) {
    reset_feedback()
    if (!(x %in% names(sub_chunks))) {
      add_feedback("code block", x, "was missing from your RMarkdown file",
                   sep = " ")
      this_code <- ""
    } else {
      this_code <- sub_chunks[[x]]
    }
    if (!use_sub_env) {
      this_env <- key[["start_env"]][[x]]
    }
    assess_task(sub_id, x, this_code,
                key[["a_code"]][[x]], this_env, use_sub_env)
  })
  names(res) <- key[["task"]]

  tibble::enframe(res, "task") %>%
    tidyr::unnest() %>%
      dplyr::mutate(sub_id = sub_id, filename = filename,
                    code = sub_chunks[key[["task"]]]) %>%
      select(sub_id, task, vars:fbk, code, filename)
}

#' Assess individual task
#'
#' Assess an individual task from a submission
#' @param sub_id
#' @param task
#' @param sub_code
#' @param a_code
#' @param orig_env
#' @export
assess_task <- function(sub_id, task, sub_code, a_code,
                        orig_env, use_sub_env = TRUE) {
  if (!use_sub_env) {
    sub_env <- new.env(parent = orig_env)
  } else {
    sub_env <- orig_env
  }
  assign("result", list(), sub_env)
  assign("current_code", sub_code, sub_env)
  fig <- ""

  ## need add_feedback() in the environment
  evaluate::evaluate(deparse(add_feedback), sub_env, new_device = FALSE)

  ## were any "forbidden" functions called?
  ## sub_code <- strsplit(sub_code, "\n")[[1]]
  forbidden <- get_blacklist()
  fb_regx <- forbidden[["regex"]]
  is_forbid <- purrr::map_lgl(fb_regx, ~ any(grepl(.x, sub_code)))
  forbid <- FALSE
  if (any(is_forbid)) {
    forbid <- TRUE
    add_feedback(
      "Certain functions should *never* be called from an RMarkdown script.",
      "These functions are unsafe, require user input, or are just unnecessary.",
      "We found the following 'forbidden' function(s) in your RMarkdown script: ",
      paste(paste0("`", forbidden[is_forbid, "fn"], "()`", collapse = ", ")))
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
  if (any(errs)) {
    fbk <- paste0("your code generated the following error(s):\n",
                  paste(purrr::map_chr(result[errs], purrr::pluck, "message"),
                        collapse = "\n"), "\n")
    add_feedback(fbk)
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

  a_vars <- tibble::enframe(unlist(get("result", sub_env)), "var", "value")

  tibble::tibble(vars = list(a_vars),
                 fig = fig,
                 forbid = forbid,
                 fbk = paste("", give_feedback(), collapse = "\n"))
}
