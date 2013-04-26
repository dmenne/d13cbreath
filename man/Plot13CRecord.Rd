\name{Plot13CRecord}
\alias{Plot13CRecord}
\title{Plot a 13C record and fit results}
\usage{
  Plot13CRecord(con, breathTestRecordID,
    showParameters = NULL, ymax = NULL, xmax = NULL,
    showName = FALSE)
}
\arguments{
  \item{con}{Open connection to SQLite database; use
  \code{OpenSqliteConnection} to connect.}

  \item{breathTestRecordID}{BreathTestRecordID in database;
  used in tables BreathTestRecord (primary),
  BreathTestParameter, BreathTestTimeSeries (foreign).}

  \item{showParameters}{The parameters to display in the
  graph as a data frame with column \code{Parameter} (t50,
  tlag,GEC) and \code{Method} (BreathID, BluckCoward,
  Ghoos). If Method is NA, all variants of Parameter are
  shown. By default, shows all variants of t50.}

  \item{ymax}{Vertical scaling; default of NULL is for
  autoscaling}

  \item{xmax}{Time axis scaling; default of NULL is for
  autoscaling}

  \item{showName}{Show full patient name and DOB, initials
  otherwise}
}
\value{
  A ggplot2 graphics
}
\description{
  Reads record from database and plots raw data, fit, and
  extracted coefficients
}
\examples{
if (exists("con")) suppressWarnings(dbDisconnect(con))
sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
unlink(sqlitePath)
CreateEmptyBreathTestDatabase(sqlitePath)
con = OpenSqliteConnection(sqlitePath)
filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
breathTestRecordID = AddBreathTestRecord(filename,con)
showParameters = data.frame(Parameter="t50",Method = c("BreathID","BluckCoward"))
Plot13CRecord(con,breathTestRecordID)
Plot13CRecord(con,breathTestRecordID, showParameters)
dbDisconnect(con)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

