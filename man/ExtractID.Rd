% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadIrisCSV.R
\name{ExtractID}
\alias{ExtractID}
\title{Extracts an ID from string IRIS CSV file}
\usage{
ExtractID(id)
}
\arguments{
\item{id}{One item from columen Identifikation, e.g. "KEK-ZH-Nr.2013-1234"}
}
\description{
First tries to extract only digits, separating these by underscore 
when there are multiple blocks. If this give a non-valid  id, returns the 
whole string without spaces and perios, hoping it makes sense.
For internal use, but may be overridden for exotic IDs
}
