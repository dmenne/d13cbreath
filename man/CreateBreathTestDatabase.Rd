\name{CreateBreathTestDatabase}
\alias{CreateBreathTestDatabase}
\title{Create and sample fill SQLite database for D13CBreath}
\usage{
  CreateBreathTestDatabase(sqlitePath)
}
\arguments{
  \item{sqlitePath}{Full filename with path to create
  database file. The file will not be overwritten if it
  exists. Use \code{getOption(GastrobaseSqlitePath)} to
  find the default path for the database.}
}
\description{
  These functions can be used for testing, or as a sample
  how to write data to the database. Using sqlite allows
  for full control of database and table creation from the
  script; creating a Microsoft Access database is not
  possible from RODBC, creating tables is possible, but "ON
  UPDATE DELETE" fails and you will need to dig deep into
  webspace to find it is not your fault.
}
\examples{
sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
unlink(sqlitePath)
CreateBreathTestDatabase(sqlitePath)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

