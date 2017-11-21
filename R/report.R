#' Produce a feedback report
#'
#' @param d a table with assessment data for a single submission
#' @param template an RMarkdown template for the report
#' @param subdir the subdirectory for reports
#' @param overwrite if exists, overwrite?
#' @param quiet 'quiet' option for rmarkdown::render
#' @param extra_params any extra parameters to pass to report (will appear in params$extra)
#' @return path to the file
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
  parms$ctbl <- credit_tbl
  parms$extra <- extra_params
  
  html <- rmarkdown::render(template,
                            rmarkdown::pdf_document(),
                            output_file = "feedback_report.pdf",
                            output_dir = fulldir,
                            params = parms,
                            quiet = quiet)
}
