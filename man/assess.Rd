% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assess.R
\name{assess}
\alias{assess}
\title{Assess a submission}
\usage{
assess(
  filename,
  sub_id = filename,
  key,
  use_sub_env = TRUE,
  workdir = NULL,
  seed = NULL,
  preseed = NULL,
  task_varnames = NULL
)
}
\arguments{
\item{filename}{name of submission file}

\item{sub_id}{unique submission identifier}

\item{key}{key table (see \code{\link{compile_key}})}

\item{use_sub_env}{whether each submission code chunk is to be run
within the submission environment (to allow for dependencies
between chunks) or whether to be run in the solution environment
of the previous chunk (to allow recovery from errors).  In the
former case, all but the first of the `key$start_env`
environments will be ignored.}

\item{workdir}{the working directory in which to evaluate the code}

\item{seed}{starting seed for random number generation (or NULL to not set the seed)}

\item{preseed}{A named list whose names are the task names and whose elements are the RNG seeds to set prior to the task.}

\item{task_varnames}{A named list of task variable names, where the names are are some or all of the task names and the elements are named character vectors of variables for the corresponding task.}
}
\value{
A table
}
\description{
Assess a submission
}
\details{
Task variables are passed to the \code{task_varnames} argument to \code{\link{assess_task}}, and appear in the submission/assessment environment as the variable \code{._av}.
}
