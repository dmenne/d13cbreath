\name{tLagMaes}
\alias{tLagMaes}
\title{Determine tlag from uncorrected fit to the beta exponential function
Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6}
\usage{
  tLagMaes(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required Note that \code{k} is measured
  in 1/min (e.g. 0.01/min), usually it is quoted as 1/h
  (e.g. 0.6/h).}
}
\value{
  lag time as defined from Maes fit
}
\description{
  Determine tlag from uncorrected fit to the beta
  exponential function Maes B D, Ghoos Y F, Rutgeerts P J,
  Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci.
  39 S104-6
}
\seealso{
  \code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}}
  for an example.
}

