% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tail.R
\name{tail_linear}
\alias{tail_linear}
\title{tail_linear}
\usage{
tail_linear(idf_, n_points = 2, cutoff = 25)
}
\arguments{
\item{idf_}{an object of class \code{idf_}}

\item{n_points}{number of idf points to fit.  The most mature points will be
used in the fit}

\item{cutoff}{last age that should have a none 1.0 development factor.  All
loss development factors with an age greater than \code{cutoff} will be set to
1.0.}
}
\description{
fit a linear tail factor to ldfs
}
\examples{

my_idf <- idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)

test <- tail_linear(my_idf, cutoff = 10)

}
