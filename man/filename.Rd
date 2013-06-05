\docType{data}
\name{filename}
\alias{filename}
\title{Read BreathID file}
\format{chr "C:/Users/Dieter/Documents/Gastrobase2/breathID/350_20039_0_GER.txt"}
\usage{
  filename
}
\arguments{
  \item{filename}{Name of TXT-file to be read}
}
\value{
  structure of class \code{\link{BreathTestData}}
}
\description{
  Reads 13C data from a BreathId file, and returns a
  stucture for use by
  \code{\link{BreathTestRecordToDatabase}}.
}
\examples{
filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
bid = ReadBreathId(filename)
str(bid)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}
\keyword{datasets}

