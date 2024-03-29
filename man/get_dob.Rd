% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dob.R
\name{get_dob}
\alias{get_dob}
\title{Get the date of birth}
\usage{
get_dob(tgt, which = "00")
}
\arguments{
\item{tgt}{A list with elements \code{psn} and \code{xyz}}

\item{which}{String: Child ("00"), father ("01") or mother ("02")}
}
\value{
Object of class \code{Date}. If dob is missing,
the function return dob 01-01-2000 (for child).
}
\description{
Get the date of birth
}
\note{
Function is local to the \code{bdsreader} package, not exported.
}
