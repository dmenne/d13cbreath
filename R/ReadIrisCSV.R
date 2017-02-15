#' @title Read 13C data from IRIS/Wagner Analysen in CSV Format
#'
#' @description Reads 13C data from IRIS/Wagner Analysen in CSV Format
#' The composite files starts as follows:
#' \preformatted{
#' "Name","Vorname","Test","Identifikation"
#'} 
#' This format does not have information about the substrate (acetate, octanoate),
#' the dose and body weight and height. Acetate as substrate, Dose = 100, 
#' Weight = 75, Height = 180  is assumed.
#' @param filename Name of IRIS/Wagner file in csv format
#' @return list with \code{FileName, PatientName, PatientFirstName, 
#' Test, Identifikation},
#' and data frame \code{Data} with \code{Time} and \code{DOB}
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' filename = system.file("extdata", "IrisCSV.TXT", package = "D13CBreath")
#' irisData = ReadIrisCSV(filename)
#' str(irisData)
#' @export ReadIrisCSV
ReadIrisCSV = function(filename) {
  if (!file.exists(filename))
    stop(paste0("File ",filename," does not exist."))
  # Check if this is the right format
  header = readLines(filename, 1)
  if (str_detect(header,("^\"Name\",\"Vorname\",\"Test\",\"Identifikation\"")) != 1)
    stop(
      paste0(
        "File ",filename,
        " is not a valid IRIS CSV file. First line should be Name, Vorname,..."
      )
    )
  d = suppressWarnings(readr::read_csv(filename, col_types = "cccciddddddcc",
                        locale = readr::locale(encoding = "ISO_8859-2")))
  readr::stop_for_problems(d)
  if (ncol(d) != 13)  
    stop(paste0("IRIS CSV file ", filename, " has unexpected columns. Should be 13"))
  if (nrow(d) < 5) 
    stop(paste0("IRIS CSV File ", filename, " has only ", nrow(d), " rows"))

  RecordDate = strptime(d$Datum[1],"%d.%m.%Y")
  PatientID = extract_id(d$Identifikation[1])
  TestNo = d$Test[1]
  # There are multiple "Name" fields; skip the first
  Name = d$Name[1]
  FirstName = d$Vorname[1]
  Initials = NA
  if (nchar(Name) > 0 && nchar(FirstName) > 0)
    Initials =  paste0(str_sub(Name,1,1),
                      str_sub(FirstName,1,1))
  data = try(d[,c("Testzeit[min]","DOB [o/oo]")], silent = TRUE)
  if (inherits(data,"try-error"))
    stop("Invalid data columns in Iris data file")
  names(data) = c("Time","DOB")
  # remove too small values
  data = data[data$DOB >= -10,]
  BreathTestData(
    FileName = basename(filename),
    PatientID = PatientID,
    Name = Name,
    FirstName = FirstName,
    Initials =  Initials,
    TestNo = 9999,
    Dose = 100,
    Study = TestNo,
    RecordDate = RecordDate,
    Height = 180,
    Weight = 75,
    Device = "Iris",
    Substrate = 'acetate',
    Data = as.data.frame(data)
  )
}


#' @title Extracts an ID from string IRIS CSV file
#'
#' @description First tries to extract only digits, separating these by underscore 
#' when there are multiple blocks. If this give a non-valid  id, returns the 
#' whole string without spaces and perios, hoping it makes sense.
#' For internal use, but may be overridden for exotic IDs
#' @param id One item from columen Identifikation, e.g. "KEK-ZH-Nr.2013-1234"
#' @export
extract_id = function(id){
  id1 = paste(str_match_all(id, "([\\d]+)")[[1]][,2], collapse = "_")
  if (nchar(id1) >= 5) return(id1)
  tolower(str_replace_all(id, "[\\.\\-\\W]+", "_"))
}

