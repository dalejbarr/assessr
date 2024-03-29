% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fn_regex}
\alias{fn_regex}
\title{Generate regular expression to search for R function call}
\usage{
fn_regex(fname)
}
\arguments{
\item{fname}{name of the function}
}
\value{
a string containing a regular expression
}
\description{
Generate a regular expression to use in a search function, to find a function call in R code.
}
