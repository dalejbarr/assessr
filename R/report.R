reformat_long_lines <- function(x, cutoff = 75L, notify_reformat = TRUE) {
  if (cutoff == -1) {
    x
  } else {
    src <- evaluate::parse_all(x, allow_error = TRUE)[["src"]]
    lens <- purrr::map(strsplit(src, "\n"), ~ map_int(.x, nchar))
    todo <- purrr::map_lgl(lens, ~ any(.x > cutoff))
    src[todo] <- purrr::map(src[todo], function(ff) {
      reformat <- paste(formatR::tidy_source(text = ff,
                                             comment = FALSE,
                                             indent = 2,
                                             width.cutoff = cutoff,
                                             output = FALSE)$text.tidy,
                        collapse = "\n")
      if (notify_reformat) {
        paste0("# NOTE: the line below was too wide and has been reformatted to fit\n",
               reformat)
      } else {
        reformat
      }
    })
    unlist(src)
  }
}

#' Produce feedback reports
#'
#' @param ares assessment result
#' @param template an RMarkdown template for the report
#' @param subdir the subdirectory for reports
#' @param overwrite if exists, overwrite?
#' @param quiet 'quiet' option for rmarkdown::render
#' @param extra_params any extra parameters to pass to report (will appear in params$extra)
#' @param long_line_cutoff reformat chunks where any single line is longer than this value (-1 = don't reformat)
#' @param empty_fbk default feedback (when \code{fbk} field is empty)
#' @param stop_after stop processing after N (-1 to process all)
#' @return vector of report filenames
#' @export
feedback_all <- function(ares,
                         template,
                         subdir = "feedback_reports",
                         overwrite = FALSE,
                         quiet = TRUE,
                         extra_params = NULL,
                         long_line_cutoff = 75,
                         empty_fbk = "* no issues",
                         stop_after = -1L) {

  safe_report <- purrr::safely(feedback_report)
  
  ar2 <- ares %>%
    group_by(sub_id, filename) %>%
    nest()
  if (stop_after != -1L) {
    ar2 <- slice(ar2, 1:stop_after)
  }
  n_todo <- length(ar2[["data"]])
  todo <- list(ar2[["data"]], ar2[["filename"]],
               template, ar2[["sub_id"]],
               seq_along(ar2[["data"]]))
  result <- purrr::pmap(todo,
              function(.x, .y, tpl, sid, ix) {
                message("Processing ", ix, " of ", n_todo,
                        " (", sid, ")")
                safe_report(.x, .y, tpl, subdir,
                                overwrite, quiet, extra_params,
                                long_line_cutoff, empty_fbk)
              })
  ## TODO: produce warnings
  fnames <- purrr::map(result, function(x) {x[["result"]]})
  errs <- purrr::map(result, function(x) {x[["error"]]})
  tibble(sub_id = ar2[["sub_id"]],
         compiled = !purrr::map_lgl(fnames, is.null),
         filename = purrr::map_chr(fnames, ~ if (is.null(.x)) "" else .x),
         error = errs)
}

#' Produce a feedback report
#'
#' @param d a table with assessment data for a single submission
#' @param filename filename of the submission
#' @param template an RMarkdown template for the report
#' @param subdir the subdirectory for reports
#' @param overwrite if exists, overwrite?
#' @param quiet 'quiet' option for rmarkdown::render
#' @param extra_params any extra parameters to pass to report (will appear in params$extra)
#' @param long_line_cutoff reformat chunks where any single line is longer than this value (-1 = don't reformat)
#' @param empty_fbk default feedback (when \code{fbk} field is empty)
#' @return path to the report
#' @importFrom magrittr %>%
#' @export
feedback_report <- function(d,
                            filename,
                            template,
                            subdir = "feedback_reports",
                            overwrite = FALSE,
                            quiet = TRUE,
                            extra_params = NULL,
                            long_line_cutoff = 75,
                            empty_fbk = "* no issues") {
  if (subdir == "") {
    stop("'subdir' cannot be an empty string")
  }
  fulldir <- file.path(sub("/+$", "", subdir), # trailing /
                       sub("(^.+/|^\\.$)", "", dirname(filename)))
  if (dir.exists(fulldir)) {
    if (!overwrite) {
      stop("'", fulldir, "' exists and 'overwrite' is FALSE")
    } else {
      unlink(fulldir, recursive = TRUE)
    }
  }
  dir.create(fulldir, FALSE, TRUE)

  parms <- list()
  parms$code <- purrr::map(d$code, reformat_long_lines,
                           cutoff = long_line_cutoff, notify_reformat = TRUE)
  names(parms$code) <- d$task

  parms$fbk <- as.list(ifelse(d$fbk == "", empty_fbk, d$fbk))
  names(parms$fbk) <- d$task

  parms$fig <- map(d$fig, function(.x) {
    ## if (nchar(.x) > 23) {
    if (substr(.x, 1, 10) == "data:image") {
      fig2 <- substr(.x, 23, nchar(.x))
      t <- tempfile(fileext = ".png")
      writeBin(base64enc::base64decode(fig2), t)
      paste0("\\includegraphics[width = .5\\textwidth]{", t, "}")
    } else {
      .x
    }
  })
  names(parms$fig) <- d$task
  
  credit_tbl <- 
    d[, c("task", "vars")] %>%
    tidyr::unnest() %>%
    dplyr::group_by(task) %>%
    dplyr::summarise(credit = dplyr::case_when(sum(value) == n() ~ "Full",
                                               sum(value) > 0 ~ "Partial",
                                               TRUE ~ "None"))
  ##parms$ctbl <- credit_tbl
  parms$extra <- extra_params
  
  rmarkdown::render(template,
                    rmarkdown::pdf_document(
                                 includes =
                                   rmarkdown::includes(in_header = "header.tex")),
                    output_file = "feedback_report.pdf",
                    output_dir = fulldir,
                    params = parms,
                    quiet = quiet)
}

#' Create a feedback template for the assessment
#'
#' @param a_file rmarkdown file with assessment code
#' @param s_file rmarkdown file with solutions
#' @param o_file name of output file
#' @param overwrite overwrite the file if it exists
#' @return name of the output file
feedback_template <- function(a_file,
                              s_file,
                              o_file = "feedback_template.Rmd",
                              overwrite = FALSE) {
  if (file.exists(o_file) && !overwrite) {
    stop("output file '", o_file, "' exists and overwrite = FALSE")
  }

  if (!file.exists(a_file)) {
    stop("couldn't find assessment file '", a_file, "'")
  }

  if (!file.exists(s_file)) {
    stop("couldn't find solution file '", s_file, "'")
  }

  code <- tangle(a_file)
  s_code <- tangle(s_file)

  cat("---\ntitle: Feedback Report\nauthor: Teaching Team\n",
      "params:",
      "  code:  !r list()",
      "  fbk:   !r list()",
      "  extra: !r list()",
      "  fig:   !r list()",
      "---", sep = "\n", file = o_file)

  cat("\n", file = o_file, append = TRUE)

  cat("```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = TRUE)", "```",
      sep = "\n", file = o_file, append = TRUE)

  cat("\n", file = o_file, append = TRUE)

  purrr::walk(names(code), function(x) {
    cat("## Task\n\n", file = o_file, append = TRUE)

    cat("\\renewenvironment{Shaded}{\\begin{subcode}}{\\end{subcode}}\n\n",
        file = o_file, append = TRUE)

    cat(
      rmd_chunk_stub(paste0(x, "_sub"),
                     list(eval = FALSE, code = paste0("params$code$", x)),
                     trailing = ""),
      sep = "\n", file = o_file, append = TRUE)

    cat("\\renewenvironment{Shaded}{\\begin{solcode}}{\\end{solcode}}\n\n",
        file = o_file, append = TRUE)

    cat(rmd_chunk_head(paste0(x, "_sol"),
                       list(eval = FALSE)),
        sep = "\n", file = o_file, append = TRUE)
    cat(s_code[[x]], "```", sep = "\n", file = o_file, append = TRUE)
    cat("\n", file = o_file, append = TRUE)

    cat(
      rmd_chunk_head(paste0(x, "_fbk"),
                     list(results = "'asis'", echo = FALSE)),
      sep = "\n", file = o_file, append = TRUE)
    cat(paste0("cat(paste0(\"> \", params$fbk$", x, "))\n```\n\n"),
        file = o_file, append = TRUE)
    
    cat("\n", file = o_file, append = TRUE)
  })

  message("Wrote code chunks for ", length(code),
          " assessed ",
          if (length(code) > 1) "blocks" else "block",
          " to file '", o_file, "'")
  
  return(invisible(o_file))
}

rmd_chunk_head <- function(chunk_name, params = list()) {
  chead <- paste0("```{r ", chunk_name)
  cmid <- ""
  if (length(params)) {
    cmid <- paste0(", ", paste(paste(names(params), "=", params), collapse = ", "))
  }
  paste0(chead, cmid, "}")
}

rmd_chunk_stub <- function(chunk_name, params = list(), trailing = "\n") {
  paste(rmd_chunk_head(chunk_name, params), "```",
        trailing,
        sep = "\n")
}

