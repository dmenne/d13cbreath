#' @title Read 13C data from IRIs/Wagner Analysen
#' 
#' @description Reads 13C data from IRIS/Wagner Analysen. 
#' Reading CSV-formatted files is not supported, because these do not contain
#' weight and height information required for DOB to PDR conversion.
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
#' filename = system.file("extdata", "IrisMulti.txt", package = "D13CBreath")
#' irisData = ReadIris(filename)
#' str(irisData)
#' @export ReadIris
ReadIris = function(filename) {
  if (!file.exists(filename)) 
    stop(str_c("File ",filename," does not exist."))
  # Check if this is the right format
  bid = readLines(filename)
  header = str_trim(bid[1])
  if (header != "\"Testergebnis\"")
    stop(str_c("File ",filename,
      " is not a valid Iris file. First line should be <<Testergebnis>>")) 
  dataRow = which(str_detect(bid,"Daten"))
  if (length(dataRow)==0 )
    stop("File does not contain data" ) 
  
  recordDate = findPattern(bid,"Datum")
  RecordDate = strptime(recordDate,"%d.%m.%Y")
  # Try if there is a Patient number. If not, try Identification
  PatientID = try(findPattern(bid,"Patient"), silent=TRUE)
  if (class(PatientID) == "try-error")  
    PatientID = findPattern(bid,"Identifikation")
  
  TestNo = as.integer(findPattern(bid,"Nummer"))
  Substrate = findPattern(bid,"Substrat")
  Gender = findPattern(bid,"Geschlecht")
  if (nchar(Gender)> 0) {
    Gender = str_sub(tolower(Gender),1,1)
    if (Gender != "m") Gender ="f" # Make sure to avoid German names
  }
  Dose = as.numeric(findPattern(bid,"Dosis"))
  # Workaround for "Größe" and UTF
  Height = as.numeric(findPattern(bid,"Gr.*e.*",TRUE))*100
  Weight = as.numeric(findPattern(bid,"Gewicht.*",TRUE))  
  Test = findPattern(bid,"Abk.*rzung")
  # There are multiple "Name" fields; skip the first
  Name = findPattern(bid[-(1:14)],"Name")
  FirstName = findPattern(bid,"Vorname")
  Initials = NA
  if (nchar(Name)>0 && nchar(FirstName)>0)    
    Initials =  str_c(str_sub(Name,1,1), 
                      str_sub(FirstName,1,1))
  data = read.csv(textConnection(bid[-(1:dataRow)]))
  data = try(data[,c("Testzeit..min.","DOB..o.oo.","Atom.ppm.Excess.13C..ppm.")])
  data = try(data[,c("Testzeit..min.","DOB..o.oo.")])
  if (inherits(data,"try-error"))
    stop("Invalid data columns in Iris data file")
  names(data) = c("Time","DOB")
  BreathTestData(
    FileName=basename(filename),
    PatientID=PatientID,
    Name = Name,
    FirstName = FirstName,
    Initials =  Initials,
    TestNo=TestNo,
    Study = Test,
    RecordDate = RecordDate,
    Device = "Iris",
    Height= Height,
    Weight = Weight,
    Substrate=Substrate,
    Data=data)
}


findPattern = function(bid,pattern,required=TRUE){
  p = str_match(bid,str_c('\\"',pattern,'\\",\\s*\\"(.*)\\"'))[,2]
  p = p[!is.na(p)]
  if (length(p)>1) 
    stop(str_c("No unique <<", pattern,">> in Iris file"))
  if (length(p)==0){
    if (required)
      stop(str_c("No <<" ,pattern, ">> found in Iris file "))
    else 
      p=""
  }
  return(str_trim(p))
}
