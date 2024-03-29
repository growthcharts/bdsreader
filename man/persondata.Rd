% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/persondata.R
\name{persondata}
\alias{persondata}
\title{Extract person-level information}
\usage{
persondata(x)
}
\arguments{
\item{x}{A list containing element \code{psn}}
}
\value{
A tibble with person-level data
}
\description{
Extract person-level information
}
\examples{
fn <- system.file("extdata/bds_v3.0/smocc/Laura_S.json",
  package = "jamesdemodata")
persondata(read_bds(fn))
}
