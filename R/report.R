#' Produce a feedback report
#'
#' @param d a table with assessment data for a single submission
#' @param filename filename of the submission
#' @param template an RMarkdown template for the report
#' @param subdir the subdirectory for reports
#' @param overwrite if exists, overwrite?
#' @param quiet 'quiet' option for rmarkdown::render
#' @param extra_params any extra parameters to pass to report (will appear in params$extra)
#' @return path to the report
#' @importFrom magrittr %>%
#' @export
feedback_report <- function(d,
                            filename,
                            template,
                            subdir = "feedback_reports",
                            overwrite = FALSE,
                            quiet = TRUE,
                            extra_params = NULL) {
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
  parms$code <- d$code
  names(parms$code) <- d$task

  parms$fbk <- as.list(d$fbk)
  names(parms$fbk) <- d$task

  credit_tbl <- 
    d[, c("task", "vars")] %>%
    tidyr::unnest() %>%
    dplyr::group_by(task) %>%
    dplyr::summarise(credit = dplyr::case_when(sum(value) == n() ~ "Full",
                                               sum(value) > 0 ~ "Partial",
                                               TRUE ~ "None"))
  ##parms$ctbl <- credit_tbl
  parms$extra <- extra_params
  
  html <- rmarkdown::render(template,
                            rmarkdown::pdf_document(),
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
      "params:", "  code:  !r list()", "  fbk:   !r list()",
      "  extra: !r list()",
      "---", sep = "\n", file = o_file)

  cat("\n", file = o_file, append = TRUE)

  cat("```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = TRUE)", "```",
      sep = "\n", file = o_file, append = TRUE)

  cat("\n", file = o_file, append = TRUE)

  purrr::walk(names(code), function(x) {
    cat("## Task\n\n", file = o_file, append = TRUE)

    cat("### Your code and feedback\n\n", file = o_file, append = TRUE)

    cat(
      rmd_chunk_stub(paste0(x, "_sub"),
                     list(eval = FALSE, code = paste0("params$code$", x)),
                     trailing = ""),
      sep = "\n", file = o_file, append = TRUE)

    cat(
      rmd_chunk_head(paste0(x, "_fbk"),
                     list(results = "'asis'", echo = FALSE)),
      sep = "\n", file = o_file, append = TRUE)
    cat(paste0("cat(params$fbk$", x, ")\n```\n\n"),
        file = o_file, append = TRUE)

    cat("### Solution\n\n", file = o_file, append = TRUE)
    cat(rmd_chunk_head(paste0(x, "_sol"),
                       list(eval = FALSE)),
        sep = "\n", file = o_file, append = TRUE)
    cat(s_code[[x]], "```", sep = "\n", file = o_file, append = TRUE)
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
