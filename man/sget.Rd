% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sget}
\alias{sget}
\title{Safely return object}
\usage{
sget(.obj, .class, env)
}
\arguments{
\item{object}{name of variable}

\item{classes}{type of object}
}
\value{
a list with elements \code{obj}, the object (if it exists, NULL otherwise), \code{exists} (TRUE if it does, FALSE if not), \code{type} (TRUE if object is of type 'class', otherwise FALSE), and \code{message}, a character vector describing any errors (or NULL on success)
}
\description{
Safely return object
}
