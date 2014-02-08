\name{RebuildFitDatabase}
\alias{RebuildFitDatabase}
\title{Recompute all fit parameters}
\usage{
RebuildFitDatabase(con = NULL)
}
\arguments{
  \item{con}{Connection to sqlite database; if missing,
  default database in path
  \code{getOption("Gastrobase2SqlitePath")} is used.}
}
\description{
Recomputes all fits parameters, excluding population fit.
Use this function to refresh coefficients when the
algorithm has changed.
}

