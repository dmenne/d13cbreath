\name{BluckCoward2}
\alias{BluckCoward2}
\title{Equation (2), page 4 from Bluck, Recent advances in the interpretation of
the 13C octanoate breath test for gastric emptying}
\usage{
  BluckCoward2(Time, Dose, cf)
}
\arguments{
  \item{Time}{in minutes}

  \item{Dose}{in mg}

  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required}
}
\value{
  vector of predicted cumulative PDR
}
\description{
  Equation (2), page 4 from Bluck, Recent advances in the
  interpretation of the 13C octanoate breath test for
  gastric emptying
}
\seealso{
  \code{\link{ExpBeta}}
}

