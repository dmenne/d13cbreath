\name{CumExpBeta}
\alias{CumExpBeta}
\title{Cumulative Exponential Beta function}
\usage{
CumExpBeta(Time, Dose, cf)
}
\arguments{
  \item{Time}{in minutes}

  \item{Dose}{in mg}

  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required. Note that \code{k} is measured
  in 1/min (e.g. 0.01/min), usually it is quoted as 1/h
  (e.g. 0.6/h).}
}
\value{
vector of predicted cumulative PDR
}
\description{
Equation (2), page 4 from Bluck, "Recent advances in the
interpretation of the 13C octanoate breath test for gastric
emptying". This is the cumulative beta exponential, and can
be used to compute the 50% points.
}
\seealso{
\code{\link{ExpBeta}}
}

