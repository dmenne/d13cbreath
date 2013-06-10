\name{BluckCoward2}
\alias{BluckCoward2}
\title{Bluck-Coward self-correcting t50 from 13C breath test}
\usage{
  BluckCoward2(Time, Dose, cf)
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
  interpretation of the 13C octanoate breath test for
  gastric emptying", solved for time of 50%. See also
  equation G(n,t) in "Bluck, LJC, Jackson S, Vlasakakis G,
  Mander A (2011) Bayesian hierarchical methods to
  interpret the 13C-octanoic acid breath test for gastric
  emptying. Digestion 83_96-107, page 98.
}
\seealso{
  \code{\link{ExpBeta}}
}

