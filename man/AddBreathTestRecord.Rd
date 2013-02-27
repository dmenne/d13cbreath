\name{AddBreathTestRecord}
\alias{AddBreathTestRecord}
\title{Reads record from file and writes it to the database}
\usage{
  AddBreathTestRecord(filename, con)
}
\arguments{
  \item{filename}{Name of BreathID file}

  \item{con}{Connection to sqlite database}
}
\description{
  Reads BreathID data record, computes several fit
  parameters and a fit, and writes these to the database
}
\examples{
if (exists("con")) suppressWarnings(dbDisconnect(con))
sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
unlink(sqlitePath)
CreateBreathTestDatabase(sqlitePath)
con = OpenSqliteConnection(sqlitePath)
filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
AddBreathTestRecord(filename,con)
dbDisconnect(con)
}

