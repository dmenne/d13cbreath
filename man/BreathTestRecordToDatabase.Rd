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
  BreathTestRecordID of added record
}
\description{
  Appends a record to the database.

  Table Patient: Creates patient if required.

  Table BreathTestRecord: PatientID (refers to Patient);
  Filename, Device, ...

  Table BreathTestTimeSeries: Original times series as
  \code{Parameter=BreathID},
}

