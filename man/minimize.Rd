% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{minimize}
\alias{minimize}
\title{Minimize a selection list of top level containers}
\usage{
minimize(dat, fraction = 0.1)
}
\arguments{
\item{dat}{A data frame of observations to reduce.}

\item{fraction}{Numeric scalar between 0 and 1 representing the fraction to
retain. (Default 0.1 for 10 percent.)}
}
\value{
A data frame of observations randomly reduced.
}
\description{
This reduces the total number of aliquots to audit as defined by parameter
fraction. It applies multiple times through the standard workflow. This is an
internal function for other functions within FreezerCheck. A random selection
of the target data set is retained. Reduction focus takes place elsewhere.
}
\examples{
to_reduce <- demo_data[1:125, ]
minimize(to_reduce)
minimize(to_reduce, 0.25)
}
