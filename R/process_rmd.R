knit_safely <- purrr::safely(knitr::knit)
render_safely <- purrr::safely(rmarkdown::render, "")

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
#' @param workdir working directory
#' @param savefig save any files created in solution subdirectory?
#' @details Sets up environments for running submission code and
#'   assessment code.  Also creates output/plots when appropriate and
#'   stores them in the \code{solution} subdirectory in the current
#'   working directory.
#' @return a list containing the starting environments for each task
#' @seealso \code{\link{tangle}}
#' @export
compile_key <- function(s_file, a_file, overwrite = FALSE,
                        workdir = NULL, save_fig = TRUE) {
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

  ## check if solution directory exists and if so,
  ## delete if overwrite is TRUE
  if (save_fig) {
    if (dir.exists("solution")) {
      if (overwrite) {
        unlink("solution", recursive = TRUE)
      } else {
        stop("directory 'solution' exists and overwrite = FALSE")
      }
    }
    dir.create("solution")
  }
  
  lastchunk <- 0L
  oldwd <- getwd()
  if (!is.null(workdir)) {
    setwd(workdir) # living dangerously
  }
  for (i in seq_along(tasks_ix)) {
    this_env <- new.env(parent = lastenv)
    to_run <- setdiff((lastchunk + 1L):tasks_ix[i], tasks_ix[i])
    todo <- paste(unlist(sol_chunks[to_run]), collapse = "\n")
    
    imgfile <- tempfile(fileext = ".png")
    png(imgfile)
    res <- evaluate::evaluate(todo, envir = this_env, new_device = FALSE,
                              stop_on_error = 0L)
    dev.off()
    if (file.exists(imgfile) && save_fig) {      
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
      setwd(oldwd)
      stop(geterrmessage(res[[ix]]))
    }
    lastenv <- this_env
    lastchunk <- to_run[length(to_run)]
    starting_env[[i]] <- this_env
  }
  setwd(oldwd)

  names(starting_env) <- c(tasks, "")
  solution_envs <- starting_env[-1]
  names(solution_envs) <- tasks

  tibble::tibble(task = tasks,
                 s_code = sol_chunks[tasks],
                 a_code = a_chunks[tasks],
                 start_env = starting_env[-length(starting_env)],
                 sol_env = solution_envs,
                 figpath = figlist)
}

#' Remove blacklisted functions
#'
#' Comment out any blacklisted functions that appear within a file
#'
#' @param subfile path to submission RMarkdown file
#' @return a tibble with columns \code{blacklist}, whether any functions were found; and \code{bfns}, names of these functions
#' @details warning: the original file will be overwritten, with any
#'   lines containing blacklisted code commented out
#' @seealso \code{\link{get_blacklist}}
#' @export
apply_blacklist <- function(subfile) {
  lines <- readLines(subfile)
  chunk_0 <- grep(knitr::all_patterns$md$chunk.begin, lines)
  chunk_1 <- grep(knitr::all_patterns$md$chunk.end, lines)
  ## TODO: something with the inline code
  inline <- grep("`r .+`", lines)
  msg <- paste0("Malformed RMarkdown file ", subfile, " :\n   ")
  skip <- FALSE
  if ((length(chunk_0) != length(chunk_1)) || (length(chunk_0) == 0)) {
    skip <- TRUE
    if (length(chunk_0) == 0) {
      warning(msg, "No chunks found.")
    } else {
      warning(msg, "File contains ", length(chunk_0),
           " markups for starting chunks, ",
           "but ", length(chunk_1), " markups for ending chunks.")
    }
  }
  fb_regx <- get_blacklist()[["regex"]]

  ## check the chunks
  blist <- purrr::map2(chunk_0, chunk_1, function(c0, c1) {
    if (c1 <= c0) {
      stop(msg, "Opening and closing chunk markers do not match")
    }
    lines2 <- sub("#.+$", "", lines[(c0 + 1L):(c1 - 1L)])
    ## detect any blacklisted functions
    purrr::map(lines2, function(x) {
      gg <- purrr::map(seq_along(fb_regx), function(i) {
        if (grepl(fb_regx[i], x)) i else NULL
      }) %>% purrr::compact() %>% unlist()
    })
  })

  ## now comment out the bad functions
  newchk <- purrr::pmap(list(chunk_0, chunk_1, blist), function(c0, c1, b) {
    if (purrr::some(b, ~ !is.null(.x))) {
      lmid <- lines[(c0 + 1L):(c1 - 1L)]
      lfix <- purrr::map2_chr(b, lmid, function(fs, ll) {
        if (is.null(fs)) {
          ll
        } else {
          reduce(fs, ~ gsub(fb_regx[.y], "## \\2(", .x), .init = ll)
        }
      })
      c(lines[c0], lfix, lines[c1])
    } else {
      lines[c0:c1]
    }
  })

  lines_in <- purrr::map2(chunk_0, chunk_1, `:`) %>% unlist()
  lines_out <- setdiff(seq_along(lines), lines_in)

  newlines <- tibble::tibble(ix = c(lines_out, lines_in),
                 line = c(lines[lines_out], unlist(newchk))) %>%
    dplyr::arrange(ix) %>%
    `[[`("line")

  writeLines(newlines, subfile)

  bfns <- get_blacklist()$fn[purrr::flatten(blist) %>% unlist()]
  blacklist <- FALSE
  if (length(bfns)) {
    blacklist <- TRUE
  }
  tibble::tibble(blacklist = blacklist,
                 bfns = list(bfns))
}

#' Compile assignment report
#'
#' Safely compile assignment report from submitted RMarkdown script
#'
#' @param subfile path to submission RMarkdown file
#' @param quiet whether to run quietly (suppress messages)
#' @param blacklist comment out lines of code containing blacklisted functions
#' @details runs a safe version of \code{\link{rmarkdown::render}} on
#'   the submision file
#' @export
compile_assignment <- function(subfile,
                               quiet = FALSE,
                               blacklist = TRUE) {

  if (blacklist) {
    blist <- apply_blacklist(subfile)
  } else {
    blist <- NULL
  }

  ##browser()
  if (quiet) {
    sink(tempfile())
    stime <- system.time(
      suppressMessages(
        res <- render_safely(subfile,
                             rmarkdown::html_document(),
                             output_dir = dirname(subfile),
                             ##knit_root_dir = dirname(subfile),
                             quiet = quiet)))
    sink()
  } else {
    stime <- system.time(res <- render_safely(subfile,
                                              rmarkdown::html_document(),
                                              output_dir = dirname(subfile),
                                              ##knit_root_dir = dirname(subfile),
                                              quiet = quiet))
  }

  dplyr::bind_cols(
    tibble::tibble(filename = subfile,
                   html = if (is.null(res$result)) "" else res$result,
                   ctime = stime[["user.self"]],
                   err = list(res$error)),
    blist)
}

#' Compile all RMarkdown submissions
#'
#' @param subdir subdirectory containing RMarkdown submissions
#' @param quiet suppress messages while compiling
#' @param blacklist comment out lines of code containing blacklisted functions
#' @param progress report file-by-file progress
#' @param return should return table contain moodle id number
#' @return table with information about compilation for each submission
#' @details Convenience function to call \code{\link{compile_assignment}} repeatedly for each RMarkdown file in subdirectory \code{subdir}
#' @importFrom magrittr %>%
#' @export
compile_all <- function(subdir,
                        quiet = TRUE,
                        blacklist = TRUE,
                        progress = TRUE,
                        with_moodle_id = FALSE) {
  
  subs <- list_submissions(subdir)

  rtbl <- purrr::map2_df(subs, seq_along(subs), function(x, i) {
    if (progress) {
      msg1 <- sprintf("Compiling %d of %d (%s)", i, length(subs),
                      if (with_moodle_id) as.character(moodle_id(x)) else x)
      message(msg1)
    }
    compile_assignment(x, quiet, blacklist)
  })

  if (with_moodle_id) {
    rtbl %>%
      dplyr::mutate(sub_id = moodle_id(filename)) %>%
      dplyr::select(sub_id, ctime, err, blacklist, bfns, html, filename)
  } else {
    rtbl %>%
      dplyr::select(ctime, err, blacklist, bfns, html, filename)
  }
}


#' Copy data to RMarkdown submission directory
#'
#' @param subdir subdirectory of submission files
#' @param datadir subdirectory of data files
#' @details copies over data from \code{datadir} to each submission subdirectory
#' @export
copy_data <- function(subdir, datadir) {
  subs <- dirname(list_submissions(subdir))
  ddir <- sub("/$", "", datadir)
  dfiles <- list.files(datadir)
  purrr::walk(subs, function(x) {
    purrr::walk(dfiles, function(y) {
      file.copy(file.path(ddir, y), file.path(x, y))
    })
  })
}

#' List all RMarkdown submissions
#'
#' List all RMarkdown submissions in a given subdirectory
#'
#' @param subdir subdirectory in which to scan for RMarkdown files
#' @param with_moodle_id extract the moodle identifier
#' @return if \code{with_moodle_id} is \code{FALSE}, returns a vector
#'   with paths to submission files; if \code{TRUE}, a table with
#'   submission id and filename.
#' @export
list_submissions <- function(subdir,
                             with_moodle_id = FALSE) {
  files <- list.files(sub("/$", "", subdir),
                      "\\.[Rr][Mm][Dd]$", full.names = TRUE,
                      recursive = TRUE)
  if (with_moodle_id) {
    tibble::tibble(sub_id = moodle_id(files),
                   filename = files)
  } else {
    files
  }
}

