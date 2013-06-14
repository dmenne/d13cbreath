\name{UnzipBreathID}
\alias{UnzipBreathID}
\title{Unpacks a BreathID zip-File}
\usage{
  UnzipBreathID(zipPath, destinationPath,
    inZipPath = "txt/", lastZipFile = NULL,
    lastZipDate = NULL)
}
\arguments{
  \item{zipPath}{path to the zip file; the name is not
  needed. For security, nothing is read when there is more
  than in zip file in the directory. When a full path to a
  file is given and \code{lastReadDate != NULL}, the file
  is skipped if the file dates match.}

  \item{destinationPath}{path where store the unpacked
  file}

  \item{inZipPath}{partial string where to find the files
  in the zip directory. For example, if the path is
  \code{temp/BreathID/Results/txt/}, substring \code{txt/}
  is sufficient.}

  \item{lastZipFile}{file name without path of last zip
  file; if NULL, not checked}

  \item{lastZipDate}{modification date of last zip read; if
  NULL; not checked}
}
\value{
  A list with number of files \code{n},
  \code{type=c("error","info","ok")}, and message string
  \code{msg}. If \code{ok}, also a list of files that were
  extracted, the If \code{lastZipFile} and If
  \code{lastZipDate}.
}
\description{
  Retrieve Breath-ID records for a zipped file, and copies
  these to a target directors
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

