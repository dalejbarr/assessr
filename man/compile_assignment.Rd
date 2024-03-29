% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_rmd.R
\name{compile_assignment}
\alias{compile_assignment}
\title{Compile assignment report}
\usage{
compile_assignment(
  subfile,
  quiet = FALSE,
  blacklist = TRUE,
  env = parent.frame(),
  knit_root_dir = NULL,
  fix_filename = TRUE
)
}
\arguments{
\item{subfile}{path to submission RMarkdown file}

\item{quiet}{whether to run quietly (suppress messages)}

\item{blacklist}{comment out lines of code containing blacklisted functions}

\item{env}{environment in which to compile}

\item{knit_root_dir}{root directory for knitting (see \code{\link{rmarkdown::render}})}

\item{fix_filename}{replace any spaces or brackets in the filename with underscores before rendering}
}
\description{
Safely compile assignment report from submitted RMarkdown script
}
\details{
runs a safe version of \code{\link{rmarkdown::render}} on
  the submision file
}
