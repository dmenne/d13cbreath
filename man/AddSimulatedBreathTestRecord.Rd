\name{AddSimulatedBreathTestRecord}
\alias{AddSimulatedBreathTestRecord}
\title{Adds simulated breath test record to the database}
\usage{
  AddSimulatedBreathTestRecord(con)
}
\arguments{
  \item{con}{connection to sqlite database}
}
\description{
  Creates a simulated data record, computes several fit
  parameters, and appends these to the database
}
\examples{
# This example does the same as the function CreateSimulatedBreathTestDatabase
if (exists("con"))
  suppressWarnings(dbDisconnect(con))
sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
unlink(sqlitePath)
CreateEmptyBreathTestDatabase(sqlitePath)
con = OpenSqliteConnection(sqlitePath)
add = try (
  for (i in 1:10)
    AddSimulatedBreathTestRecord(con),silent = TRUE)
if (inherits(add,"try-error")) dbRollback(con) else dbCommit(con)
dbDisconnect(con)
}
\seealso{
  \code{\link{ReadBreathId}},
  \code{\link{SimulateBreathId}},
  \code{\link{CreateSimulatedBreathTestDatabase}}
}

