\name{t50Maes}
\alias{t50Maes}
\title{t50 as determined from an uncorrected fit to the beta exponential function.}
\usage{
  t50Maes(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required}
}
\value{
  time where value is 1/2 of maximum, i.e. t50 in minutes
}
\description{
  Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B
  and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6
}
\seealso{
  \code{\link{ExpBeta}}
}

