\name{RebuildPopulationFitDatabase}
\alias{RebuildPopulationFitDatabase}
\title{Recompute population fit and store results in database}
\usage{
  RebuildPopulationFitDatabase(con = NULL)
}
\arguments{
  \item{con}{connection to SQlite database. If no
  connection given, opens connection to default database
  with path given by
  \code{getOption("Gastrobase2SqlitePath")}}
}
\description{
  The population fit using nlme is computed, old results
  are removed from database and coefficients are save.
}

