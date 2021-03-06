% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HandleBreathTestDatabase.R
\name{BreathTestRecordToDatabase}
\alias{BreathTestRecordToDatabase}
\title{Computes fit and writes a 13C record and extracted parameters to databse}
\usage{
BreathTestRecordToDatabase(bid, con)
}
\arguments{
\item{bid}{Record as simulated by \code{SimulateBreathID} or \code{ReadBreathID}}

\item{con}{Connection to sqlite database}
}
\value{
BreathTestRecordID of added record, or NULL if not written.
}
\description{
Appends measured values of a record to the database. Skips saving if the
file is already in the database. To overwrite an existing file,
the old record must be manually deleted from the database.
Computes and saves the extracted parameters from ExpBeta and Wagner-Nelson Fit.

Table Patient:  Creates patient if required.

Table BreathTestRecord: PatientID (refers to Patient); Filename, Device, ...

Table BreathTestTimeSeries: Original times series as \code{Parameter=BreathID},
}
