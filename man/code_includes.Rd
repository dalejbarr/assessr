% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{code_includes}
\alias{code_includes}
\title{Test whether code includes a function}
\usage{
code_includes(fn, code, remove_comments = TRUE)
}
\arguments{
\item{fn}{function to search for}

\item{code}{the submission code (usually you pass the variable
\code{._ar$current_code})}

\item{remove_comments}{do you want to remove comments before checking?}
}
\value{
logical; \code{TRUE} if the function is found anywhere in
  the code (comments in the code are ignored).
}
\description{
Test whether the submission code includes function \code{fn}.
}
