#' @title Read 13C data from IRIS/Wagner Analysen in composite format
#'
#' @description Reads 13C data from IRIS/Wagner Analysen.
#' Use ReadIrisCSV to read IRIS data in CSV format
#' The composite files required start as follows:
#' \preformatted{
#' "Testergebnis"
#' "Nummer","1330"
#' "Datum","10.10.2013"
#' "Testart"}
#'
#' @param filename Name of IRIS/Wagner file in csv format
#' @return list with \code{FileName, PatientName, PatientFirstName, Test, Identifikation},
#' and data frame \code{Data} with \code{Time} and \code{DOB}
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' filename = system.file("extdata", "IrisMulti.TXT", package = "D13CBreath")
#' irisData = ReadIris(filename)
#' str(irisData)
#' @export ReadIris
ReadIris = function(filename) {
  if (!file.exists(filename))
    stop(paste0("File ",filename," does not exist."))
  # Check if this is the right format
  bid = readLines(filename)
  header = str_trim(bid[1])
  if (header != "\"Testergebnis\"")
    stop(
      paste0(
        "File ",filename,
        " is not a valid Iris file. First line should be <<Testergebnis>>"
      )
    )
  dataRow = which(str_detect(bid,"Daten"))
  if (length(dataRow) == 0)
    stop("File does not contain data")
  
  recordDate = findPattern(bid,"Datum")
  RecordDate = strptime(recordDate,"%d.%m.%Y")
  # Try if there is a Patient number. If not, try Identification
  PatientID = try(findPattern(bid,"Patient"), silent = TRUE)
  if (class(PatientID) == "try-error")
    PatientID = findPattern(bid,"Identifikation")
  
  TestNo = as.integer(findPattern(bid,"Nummer"))
  Substrate = findPattern(bid,"Substrat")
  Gender = findPattern(bid,"Geschlecht")
  if (nchar(Gender) > 0) {
    Gender = str_sub(tolower(Gender),1,1)
    if (Gender != "m")
      Gender = "f" # Make sure to avoid German names
  }
  Dose = as.numeric(findPattern(bid,"Dosis"))
  # Workaround for "Groesse" (with umlauts and scharf-s) and UTF
  Height = as.numeric(findPattern(bid,"Gr.*e.*",TRUE)) * 100
  Weight = as.numeric(findPattern(bid,"Gewicht.*",TRUE))
  Test = findPattern(bid,"Abk.*rzung")
  # There are multiple "Name" fields; skip the first
  Name = findPattern(bid[-(1:14)],"Name")
  FirstName = findPattern(bid,"Vorname")
  Initials = NA
  if (nchar(Name) > 0 && nchar(FirstName) > 0)
    Initials =  paste0(str_sub(Name,1,1),
                      str_sub(FirstName,1,1))
  tc = textConnection(bid[-(1:dataRow)])
  data = read.csv(tc)
  close(tc)
  data = try(data[,c("Testzeit..min.","DOB..o.oo.","Atom.ppm.Excess.13C..ppm.")])
  data = try(data[,c("Testzeit..min.","DOB..o.oo.")])
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
    TestNo = TestNo,
    Dose = Dose,
    Study = Test,
    RecordDate = RecordDate,
    Device = "Iris",
    Height = Height,
    Weight = Weight,
    Substrate = Substrate,
    Data = data
  )
}


findPattern = function(bid,pattern,required = TRUE) {
  p = str_match(bid,paste0('\\"',pattern,'\\",\\s*\\"(.*)\\"'))[,2]
  p = p[!is.na(p)]
  if (length(p) > 1)
    stop(paste0("No unique <<", pattern,">> in Iris file"))
  if (length(p) == 0) {
    if (required)
      stop(paste0("No <<" ,pattern, ">> found in Iris file "))
    else
      p = ""
  }
  return(str_trim(p))
}
