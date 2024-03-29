% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{form_fixed_match}
\alias{form_fixed_match}
\title{Do fixed terms in formula match?}
\usage{
form_fixed_match(subvar, solenv, solvar = subvar, add = TRUE)
}
\arguments{
\item{subvar}{Name of submission variable containing formula.}

\item{solenv}{Name of solution environment.}

\item{solvar}{Name of solution variable.}

\item{add}{Whether to add feedback.}
}
\value{
logical
}
\description{
Do fixed terms in formula match?
}
\details{
Can handle regular linear model formulae (\code{Y ~ X}) as well as mixed effects models (\code{Y ~ X + (X | subject)}).
}
