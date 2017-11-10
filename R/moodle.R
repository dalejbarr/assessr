#' Get Moodle identifier from filename
#'
#' @param filename Name of the file downloaded from Moodle
#' @return Moodle submission identifier (integer)
#' @export
moodle_id <- function(filename) {
  as.integer(sub("^.*_([0-9]+)_assign.*$", "\\1", filename))
}
