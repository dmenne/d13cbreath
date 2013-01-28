\name{ReadIris}
\alias{ReadIris}
\title{Read 13C data from IRIs/Wagner Analysen}
\usage{
  ReadIris(filename)
}
\arguments{
  \item{filename}{Name of IRIS/Wagner file in csv format}
}
\value{
  \code{readContrasts}
}
\description{
  Reads 13C data from IRIS/Wagner Analysen. Currently it
  only support "standard" files in csv-format.
}
\examples{
filename = system.file("extdata", "standard.txt", package = "D13CBreath")
ReadIris(filename)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

