% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldf.R
\name{idf}
\alias{idf}
\title{idf}
\usage{
idf(idfs, first_age = 1)
}
\arguments{
\item{idfs}{the incemental loss development factor to develop lossed 1 period into the
future.}

\item{first_age}{the first development age.  This must be a number between 0 and 1.}
}
\description{
Create an object of class \code{idf}
}
\examples{
idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.05, 1.03), first_age = 1)
test <- idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.05, 1.03), first_age = 0.5)

# with tail
idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 0.5) \%>\%
  tail_selected(1.0)
}
