\name{t50BluckCoward}
\alias{t50BluckCoward}
\title{Newton's method to solve BluckCoward for 1/2 to compute t_50}
\usage{
  t50BluckCoward(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required}
}
\value{
  time where value is 1/2 of maximum, i.e. t50 in minutes
}
\description{
  Newton's method to solve BluckCoward for 1/2 to compute
  t_50
}
\seealso{
  \code{\link{ExpBeta}}
}

