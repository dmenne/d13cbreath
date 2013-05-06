\name{ReadBreathId}
\alias{ReadBreathId}
\title{Read 13C data from BreathID}
\usage{
  ReadBreathId(filename)
}
\arguments{
  \item{filename}{Name of TXT-file to be read}
}
\value{
  list with fields \code{FileName, StartTime, EndTime,
  PatientNumber, Name (NA), FirstName (NA), Initials (NA),
  TestNo, Type, Dose, Height, Weight, T50, TLag, GEC}, and
  \code{Data} as a data frame of \code{Time, DOB, PDR,
  PDRfit, CPDR, CPDRfit}. Fields \code{Name, FirstName and
  Initials} are returned as NA for manual filling
}
\description{
  Reads 13C data from a BreathId file.
}
\examples{
filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
bid = ReadBreathId(filename)
str(bid)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

