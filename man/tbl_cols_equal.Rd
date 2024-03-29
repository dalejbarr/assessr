% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tbl_cols_equal}
\alias{tbl_cols_equal}
\title{Are table column values equal}
\usage{
tbl_cols_equal(
  subtbl,
  subcol,
  sol_env,
  soltbl = subtbl,
  solcol = subcol,
  ignore_order = TRUE,
  add = TRUE
)
}
\arguments{
\item{subtbl}{name of submission table}

\item{subcol}{name of column in submission table}

\item{sol_env}{solution environment}

\item{soltbl}{name of table of solution environment}

\item{solcol}{name of column in solution table}

\item{ignore_order}{should the ordering of the values be ignored?}

\item{add}{whether to add feedback}
}
\value{
logical
}
\description{
Are table column values equal
}
\details{
uses \code{\link{all.equal}} to deal with floating point values
}
