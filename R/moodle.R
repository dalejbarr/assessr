credfile <- function() {
  file.path(Sys.getenv("HOME"), ".assessr_moodle_cred")
}

get_moodle_deets <- function(url = NULL, user = NULL, pass = NULL,
                             use_cred_file = TRUE) {
  vals <- list()
  cred <- list()
  if (use_cred_file && file.exists(credfile())) {
    res <- read.dcf(credfile())
    if ((nrow(res) != 1L) && (ncol(res) != 3L))
      stop("malformed credential file")
    cred$url <- res[1, "url"]
    cred$user <- res[1, "user"]
    cred$pass <- res[1, "pass"]
  }
  vals$url <- url
  vals$user <- user
  vals$pass <- pass
  if (is.null(vals$url)) {
    vals$url <- cred$url
    if (is.null(vals$url) && interactive())
      vals$url <- readline("Enter your institution's moodle URL: ")
  }
  if (is.null(vals$user)) {
    vals$user <- cred$user
    if (is.null(vals$user) && interactive())
      vals$user <- readline("Enter your moodle username: ")
  }
  if (is.null(vals$pass)) {
    vals$pass <- cred$pass
    if (is.null(vals$pass) && interactive())
      vals$pass <- getPass::getPass(msg = "Enter your moodle password: ")
  }

  if (is.null(vals$url) || is.null(vals$user) || is.null(vals$pass))
    stop("could not get credentials (are you in an non-interactive session?)")
  
  return(vals)
}


#' Get Moodle identifier from filename
#'
#' @param filename Name of the file downloaded from Moodle
#' @return Moodle submission identifier (integer)
#' @export
moodle_id <- function(filename) {
  as.integer(sub("^.*_([0-9]+)_assign.*$", "\\1", filename))
}

#' Create a credential file for moodle login
#'
#' @param url URL of the moodle server
#' @param user moodle username
#' @param overwrite overwrite existing file
#' @details Prompts user for moodle password and creates a credential
#'   file in the users home directory
#'   (\code{.assessr_moodle_cred}). This way the user can run scripts
#'   to import data without having to log in each time.  It will test
#'   the validity of the credentials before storing them.
#'
#' You only need to run this function once.  As long as the
#' credentials are set correctly, any subsequent logins will be
#' performed on demand without prompting for username or password.
#' @export
set_moodle_credentials <- function(url = NULL,
                                   user = NULL,
                                   overwrite = FALSE) {
  if (!interactive()) {
    stop("must be in interactive mode to set credentials", call. = FALSE)
  } else {
    if (file.exists(credfile()) && !overwrite)
      stop("credential file exists and overwrite = FALSE", call. = FALSE)
  }
  vals <- get_moodle_deets(url, user, use_cred_file = FALSE)
  result <- tryCatch(login(vals$url, vals$user, vals$pass, TRUE),
                     error = function(e) e)
  if (inherits(result, "error")) {
    stop("Could not validate your credentials. Invalid credentials or network error.", call. = FALSE)
  } else {
    write.dcf(data.frame(url = vals$url,
                         user = vals$user,
                         pass = vals$pass,
                         stringsAsFactors = FALSE),
              credfile())
    Sys.chmod(credfile(), mode = "0600")
    message("successfully set credential file")
  }
  return(invisible(NULL))
}

#' Download assignments from moodle
#'
#' \code{fetch} downloads student submissions for a given assignment from a moodle server.
#' 
#' @param assign_id assignment id number; digits only
#' @param targ_dir subdirectory in which to store downloaded submissions; if \code{NULL} will be downloaded to a subdirectory of the working directory named \code{assign_id}
#' @param overwrite overwrite the subdirectory if it exists (default \code{FALSE})
#' @param url URL of moodle server (if not specified in credential file)
#' @param user name of moodle user (if not specified in credential file)
#' @details 
#' @return a tibble containing information about contents of the downloaded directory (see \code{\link{list_submissions}})
#' @seealso \code{\link{login}}, \code{\link{rmdassess::describe}}
#' @export
fetch <- function(assign_id, 
                  targ_dir = paste0("assign_", assign_id),
                  overwrite = FALSE,
                  url = NULL,
                  user = NULL) {
  
  deets <- get_moodle_deets(url, user)
  
  ## make sure assign_id is correctly specified
  if (grepl("[^[:digit:]]", as.character(assign_id))) {
    stop("argument 'assign_id' must contain only digits")
  }

  ## prevent accidental overwriting
  if (dir.exists(targ_dir)) {
    if (overwrite) {
      unlink(targ_dir, recursive = TRUE)
    } else {
      stop("target directory ", targ_dir, " exists and overwrite = FALSE")
    }
  }

  targ_url <- sprintf("%s/mod/assign/view.php?id=%s&action=downloadall",
		      deets$url, as.character(assign_id))

  targ_dir2 <- sub("/$", "", targ_dir)
  tfile <- tempfile(fileext = ".zip")

  r <- httr::GET(targ_url)

  if (r$status_code != 200) 
    stop("error connecting; check that you are using the correct url")
  else if (r$headers[["content-type"]] != "application/zip") {
    ## try again, after logging in
    login(deets$url, deets$user, deets$pass, TRUE)
    r <- httr::GET(targ_url)
  }
  if (r$headers[["content-type"]] == "application/zip") {
    message("Writing to ", targ_dir2)
    bin <- httr::content(r, "raw")
    writeBin(bin, tfile)
    unzip(tfile, exdir = targ_dir2)

    list_submissions(targ_dir, TRUE)
  } else 
    stop("error: could not fetch assignment")
  ## r$status_code == 404
  ##
  ## r$status_code == 200
  ## contains "login/index.php$"
  ##
  ## r$status_code == 200
  ## r$headers$`content-type`
  ## Content-Type: application/zip
}

login <- function(url, user, pass, quiet = FALSE) {
  ## TODO: check moodle_url for sanity
  r <- httr::POST(paste0(url, "/login/index.php"), 
                  body = list(username = user,
                              password = pass),
                  encode = "form")

  if (httr::status_code(r) != 200L) {
    stop("login attempt returned HTTP status code ", httr::status_code(r),
         call. = FALSE)
  } else {
    if (grepl("login/index.php$", r$url)) {
      stop("login to ", url, " failed", call. = FALSE)
    } else {
      if (!quiet)
        message("login to ", url, " was successful")
    }
  }

  return(invisible(NULL))
}
