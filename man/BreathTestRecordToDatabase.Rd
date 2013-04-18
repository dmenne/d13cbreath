\name{BreathTestRecordToDatabase}
\alias{BreathTestRecordToDatabase}
\title{Writes a 13C record to the database}
\usage{
  BreathTestRecordToDatabase(bid, con)
}
\arguments{
  \item{bid}{Record as simulated by \code{SimulateBreathID}
  or \code{ReadBreathID}}

  \item{con}{Connection to sqlite database}
}
\value{
  BreathTestRecordID of added record, or NULL if not
  written.
}
\description{
  Appends a record to the database. Skips saving if the
  file is already in the database. To overwrite an existing
  file, the old record must be manually deleted from the
  database.

  Table Patient: Creates patient if required.

  Table BreathTestRecord: PatientID (refers to Patient);
  Filename, Device, ...

  Table BreathTestTimeSeries: Original times series as
  \code{Parameter=BreathID},
}

