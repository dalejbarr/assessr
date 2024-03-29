% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodle.R
\name{set_moodle_credentials}
\alias{set_moodle_credentials}
\title{Create a credential file for moodle login}
\usage{
set_moodle_credentials(url = NULL, user = NULL, overwrite = FALSE)
}
\arguments{
\item{url}{URL of the moodle server}

\item{user}{moodle username}

\item{overwrite}{overwrite existing file}
}
\description{
Create a credential file for moodle login
}
\details{
Prompts user for moodle password and creates a credential
  file in the users home directory
  (\code{.assessr_moodle_cred}). This way the user can run scripts
  to import data without having to log in each time.  It will test
  the validity of the credentials before storing them.

You only need to run this function once.  As long as the
credentials are set correctly, any subsequent logins will be
performed on demand without prompting for username or password.
}
