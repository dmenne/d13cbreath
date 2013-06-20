\name{AddAllBreathTestRecords}
\alias{AddAllBreathTestRecords}
\title{Reads and saves multiple 13C Breath test records}
\usage{
  AddAllBreathTestRecords(path, con)
}
\arguments{
  \item{path}{start path for recursive search}

  \item{con}{connection to sqlite database}
}
\value{
  A dataframe with columns \code{file}, \code{basename},
  \code{recordID} (NULL if not saved) and \code{status}
  with levels \code{"saved", "skipped", "invalid"}.
}
\description{
  Reads all BreathID data records in a directory, computes
  several fit parameters and a fit, and writes these to the
  database. Files that are already in the database are
  skipped. Note only the base name is tested, so that files
  with the same name in different directories are
  considered identical without testing.
}
\examples{
if (exists("con")) suppressWarnings(dbDisconnect(con))
sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
unlink(sqlitePath)
CreateEmptyBreathTestDatabase(sqlitePath)
con = OpenSqliteConnection(sqlitePath)
path = dirname(
  system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath"))
AddAllBreathTestRecords(path,con)
dbDisconnect(con)
}

