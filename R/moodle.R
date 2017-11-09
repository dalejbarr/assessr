#' Get Moodle identifier from filename
#'
#' @param filename Name of the file downloaded from Moodle
#' @return Moodle submission identifier (integer)
#' @export
moodle_id <- function(filename) {
  as.integer(sub("^.*Participant_([0-9]+)_.*$", "\\1", filename))
}
