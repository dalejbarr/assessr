#' Extract html comment
#'
#' Extract an html comment block from a file.
#'
#' @details Will fail if either: there is more than one comment block matching either delimiter; or the ending delimiter occurs before the start delimiter.
#' @param filename name of the html or Rmd file
#' @param dbegin_regex delimiter at start of block (regular expression)
#' @param dend_regex delimiter at end of block (regular expression)
#' @param remove_leading_blanks whether to keep blank lines at the start of the content
#' @param remove_ending_blanks whether to keep blank lines at the end of the content
#' @return character vector with the lines between the delimiters
#' @export
extract_html_comment <- function(filename,
                                 dbegin_regex,
                                 dend_regex = dbegin_regex,
                                 remove_leading_blanks = TRUE,
                                 remove_trailing_blanks = TRUE) {
  lines <- readLines(filename)
  html_comment_regex0 <- "<!--[[:space:]]*"
  html_comment_regex1 <- "[[:space:]]*-->"
  dbegin <- paste0(html_comment_regex0, dbegin_regex, html_comment_regex1)
  dend <- paste0(html_comment_regex0, dend_regex, html_comment_regex1)
  t0 <- grep(dbegin, lines)
  t1 <- grep(dend, lines)
  condition <- FALSE
  if ( (length(t0) == 0L) || (length(t1) == 0L) ) {
    warning("in file '", filename, "' html comment start delimiter and/or end delimiter not matched")
    condition <- TRUE
  } else if ( (length(t0) > 1L) || (length(t1) > 1L) ) {
    warning("in file '", filename,
            "' html comment start and/or end delimiter matched more than once")
    condition <- TRUE
  } else if (t1 < t0) {
    warning("in file '", filename,
            "' end delimiter appeared before start delimiter")
    condition <- TRUE
  }
  if (!condition) {
    mlines_a <- lines[seq(t0, t1, 1L)]
    mlines_b <- sub(paste0(".*", dbegin, "[[:space:]]*"), "", mlines_a)
    mlines <- sub(paste0("[[:space:]]*", dend), "", mlines_b)

    if (remove_leading_blanks || remove_trailing_blanks) {
      ## remove leading and trailing lines
      content <- grep("^[[:space:]]*$", mlines, invert = TRUE)
      result <- ""
      if (length(content) == 0L) {
        warning("empty html comment block detected for file '", filename, "'")
      } else {
        L0 <- ifelse(remove_leading_blanks, min(content), 1L)
        L1 <- ifelse(remove_trailing_blanks, max(content), length(mlines))
        result <- mlines[L0:L1]
      }
    } else {
      result <- mlines
    }
  } else {
    result <- ""
  }
  result
}
