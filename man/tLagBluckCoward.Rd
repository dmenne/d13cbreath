\name{tLagBluckCoward}
\alias{tLagBluckCoward}
\title{Lag phase for BluckCoward self-correcting fit}
\usage{
  tLagBluckCoward(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required}
}
\value{
  lag phase in minutes (time t at which the maximum in the
  rate of change of G(t) occurs)
}
\description{
  Lag phase for BluckCoward self-correcting fit
}
\seealso{
  \code{\link{BluckCoward}}
}

