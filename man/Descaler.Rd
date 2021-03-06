% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Descaler.R
\name{Descaler}
\alias{Descaler}
\title{Invert taking logs and differences of  a time series}
\usage{
Descaler(x, y = NULL, Diff = 0, Sdiff = 0, Log = FALSE, Lag = NA)
}
\arguments{
\item{x}{time series}

\item{y}{time series used as benchmark}

\item{Diff}{number of differences to be taken}

\item{Sdiff}{number of seasonal differences to be taken}

\item{Log}{Should time series be logarithmised}

\item{Lag}{Lag for Sdiff can be specified}
}
\description{
For a series that has been logged and/or differences, this function reverses these transformations.
}
\details{
The time series used as a benchmark (y) is necessary, if regular or seasonal differences have to be inversed, because the first values of this series is used to reconstruct the original values or benchmark the new series.
}
\examples{
a = ts(rnorm(100, 100, 10), start=c(2015,1), frequency=12)
b = Scaler(a, Diff=1, Log=TRUE)
Descaler(b,a, Diff=1, Log=TRUE)
}
\author{
Daniel Ollech
}
