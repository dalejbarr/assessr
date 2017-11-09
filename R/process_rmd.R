knit_safely <- purrr::safely(knitr::knit)

#' Tangle code from Rmd file
#'
#' Extract code blocks from RMarkdown file
#'
#' @param filename name of Rmd file to tangle
#' @return A list containing file blocks
#' @export
tangle <- function(filename) {
  ofname <- tempfile(fileext = ".R")
  knitr:::knit_code$restore()
  knitr::opts_knit$set(documentation = 1L)
  kresult <- knit_safely(filename, ofname, tangle = TRUE, quiet = TRUE)
  if (is.null(kresult[["error"]])) {
    knitr::read_chunk(ofname)
    res <- knitr:::knit_code$get()
  } else {
    warning("file \"", filename,
            "\"\n   gives error:\n      ",
            kresult[["error"]][["message"]],
            "\n\n   skipping this file")
    res <- list()
  }
  knitr:::knit_code$restore()
  ##purrr::map(res, paste, collapse = "\n")
  res
}

#' Compile the assessment key
#'
#' Compile the assessment key, creating starting and solution
#' environments to use in assessment, as well as output to use in
#' feedback reports.
#' 
#' @param s_file name of Rmd solution file
#' @param a_file name of Rmd file with assessment code
#' @param overwrite overwrite \code{solution} directory if it exists?
#' @details Sets up environments for running submission code and
#'   assessment code.  Creates object \code{sol_env} in each
#'   environment which has the solution environment needed for
#'   assessment.  Also creates output/plots when appropriate and
#'   stores them in the \code{solution} subdirectory in the current
#'   working directory.
#' @return a list containing the starting environments for each task
#' @seealso \code{\link{tangle}}
#' @export
compile_key <- function(s_file, a_file, overwrite = FALSE) {
  a_chunks <- tangle(a_file)
  tasks <- names(a_chunks)
  if (any(grepl("___", tasks))) {
    stop("triple underscore '___' not allowed in code block names")
  }
  ##curwd <- getwd()
  ## setwd(working_dir)
  sol_chunks <- tangle(s_file)
  tasks_ix <- c(purrr::map_int(tasks, ~ which(names(sol_chunks) == .x)),
                which(names(sol_chunks) == tasks[length(tasks)]) + 1L)
  starting_env <- list()
  lastenv <- globalenv()
  figlist <- vector("character", length(tasks))
  names(figlist) <- tasks

  ## TODO check if solution directory exists and if so,
  ## delete if overwrite is TRUE
  if (dir.exists("solution")) {
    if (overwrite) {
      unlink("solution", recursive = TRUE)
    } else {
      stop("directory 'solution' exists and overwrite = FALSE")
    }
  }
  dir.create("solution")
  
  lastchunk <- 0L
  for (i in seq_along(tasks_ix)) {
    this_env <- new.env(parent = lastenv)
    to_run <- setdiff((lastchunk + 1L):tasks_ix[i], tasks_ix[i])
    todo <- paste(unlist(sol_chunks[to_run]), collapse = "\n")
    
    imgfile <- tempfile(fileext = ".png")
    png(imgfile)
    res <- evaluate::evaluate(todo, envir = this_env, new_device = FALSE,
                              stop_on_error = 0L)
    dev.off()
    if (file.exists(imgfile)) {      
      if (to_run[length(to_run)] %in% tasks_ix) {
        ## save it
        this_task <- names(sol_chunks)[to_run[length(to_run)]]
        if (this_task %in% names(figlist)) {
          imgoutfile <- file.path("solution", paste(this_task, "png", sep = "."))
          file.copy(imgfile, imgoutfile, overwrite = overwrite)
          figlist[this_task] <- imgoutfile
        }
      }
    }
    
    if (ix <- purrr::detect_index(res, evaluate::is.error)) {
      ## setwd(curwd)
      stop(res[[ix]][["message"]])
    }
    lastenv <- this_env
    lastchunk <- to_run[length(to_run)]
    starting_env[[i]] <- this_env
  }

  for (i in seq_len(length(starting_env) - 1L)) {
    assign("sol_env", starting_env[[i + 1L]], starting_env[[i]])
  }
  names(starting_env) <- c(tasks, "")

  tibble::tibble(task = tasks,
                 s_code = sol_chunks[tasks],
                 a_code = a_chunks[tasks],
                 start_env = starting_env[-length(starting_env)],
                 sol_env = starting_env[-1],
                 figpath = figlist)
}
