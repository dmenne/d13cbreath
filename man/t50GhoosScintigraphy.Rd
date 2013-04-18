\name{t50GhoosScintigraphy}
\alias{t50GhoosScintigraphy}
\title{t50 from Ghoos with scintigrapic correction}
\usage{
  t50GhoosScintigraphy(cf)
}
\arguments{
  \item{cf}{named vector of coefficients; only \code{k} and
  \code{beta} are required}
}
\value{
  time where value is 1/2 of maximum, i.e. t50 in minutes
}
\description{
  Computes t50 the original Ghoos way (Sanaka Nakada eq 6),
  and apply the correction from Ghoos et. al 1993 to come
  close to scintigraphic values. This is for comparison
  with published data only; there is little justification
  for using it.
}
\seealso{
  \code{\link{ExpBeta}}
}

