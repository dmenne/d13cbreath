\name{tLagBluckCoward}
\alias{tLagBluckCoward}
\title{Lag phase for BluckCoward self-correcting fit}
\usage{
tLagBluckCoward(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required Note that in D13CBreath,
  \code{k} is measured in 1/min (e.g. 0.01/min), usually it
  is quoted as 1/h (e.g. 0.6/h).}
}
\value{
lag phase in minutes (time t at which the maximum in the
rate of change of G(t) occurs)
}
\description{
Lag phase for BluckCoward self-correcting fit
}
\seealso{
\code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}} for
an example.
}

