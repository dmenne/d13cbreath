\name{MarkedRecords}
\alias{MarkedRecords}
\title{Reads Setting table}
\usage{
MarkedRecords(con = NULL)
}
\arguments{
  \item{con}{Connection to sqlite database; if missing,
  uses default database.}
}
\value{
Data frame with columns \code{SettingsID, color, PatientID,
BreathTestRecordID, RecordDate}.
}
\description{
Returns a data frame of the entries of the form
\code{<color>Item} in the settings table. If a patient was
selected, adds all records of the patient.

Example of Setting table in database: '\preformatted{ '
SettingID Value ' RedItem Patient_10574 ' GreenItem
Record_433 }
}
\examples{
databasePath = CreateSimulatedBreathTestDatabase()
con = OpenSqliteConnection(databasePath)
MarkedRecords(con)
dbDisconnect(con)
}
\author{
Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

