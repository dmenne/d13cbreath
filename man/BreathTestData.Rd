% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BreathTestData.R
\name{BreathTestData}
\alias{BreathTestData}
\title{Data structure for saving into database}
\usage{
BreathTestData(PatientID, Name = NA, FirstName = NA, Initials = NA,
  DOB = NA, BirthYear = NA, Gender = NA, Study = NA,
  PatStudyID = NA, FileName, Device = "generic", Substrate, RecordDate,
  StartTime = RecordDate, EndTime = RecordDate, TestNo, Dose = 100,
  Height = NA, Weight = NA, T50 = NA, GEC = NA, TLag = NA,
  Data = Data)
}
\arguments{
\item{PatientID}{required, string or number for unique identification in database}

\item{Name}{optional}

\item{FirstName}{optional}

\item{Initials}{optional, 2 characters, 1 number}

\item{DOB}{optional Date of birth (not to be confused with "delta over baseline)}

\item{BirthYear}{optional}

\item{Gender}{optional m or f}

\item{Study}{optional name of study; can be used in population fit}

\item{PatStudyID}{optional; Patient number within study. Does not need to be globally unique}

\item{FileName}{required; file where data were read from, or other unique string.
When data are read again, this string is tested and record is skipped when
same filename is already in database, therefore uniqueness is important. When some
record does not turn up in database after repeated reading, check if a record with
the same file name is already there, and rename the file to avoid collisions.}

\item{Device}{BreathID or Iris; default "generic"}

\item{Substrate}{Should contain string "ace" or "oct" or "okt", case insensitive. Will
be replaced by "acetate" or "octanoate"}

\item{RecordDate}{Required record date.}

\item{StartTime}{optional}

\item{EndTime}{optional}

\item{TestNo}{required integer; unique test number. Converted to integer if factor}

\item{Dose}{optional, default 100 mg}

\item{Height}{optional, in cm; when PDR must be calculated, default values are
used; see \code{\link{DOBToPDR}}}

\item{Weight}{optional, in kg}

\item{T50}{optional, only present if device computes this value}

\item{GEC}{optional, only present if device computes this value}

\item{TLag}{optional, only present if device computes this value}

\item{Data}{data frame with at least 5 rows and columns \code{Time} and one
or both of \code{DOB} or \code{PDR}. If PDR is missing, and Height, Weight and Substrate
are given, computes PDR via function DOBToPDR}
}
\description{
Generates structure of class BreathTestData with required fields
and optional fields as an intermediate between file data and the SQlite database.
All optional fields by default are NA
}
