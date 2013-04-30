#' @title Read 13C data from BreathID
#' 
#' @description Reads 13C data from a BreathId file.
#' 
#' @param filename Name of TXT-file to be read
#' @return list with fields \code{FileName, StartTime, EndTime, PatientNumber, 
#' TestNo, Type, Dose, Height, Weight, T50, TLag, GEC}, and \code{Data} as a data frame of
#' \code{Time, DOB, PDR, PDRfit,	CPDR, CPDRfit}. Fields \code{Name, FirstName and Initials}
#' are returned as NA for manual filling
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @import stringr
#' @examples
#' filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
#' bid = ReadBreathId(filename)
#' str(bid)
#' @export
ReadBreathId = function(filename) {
  filename = as.character(filename)
  if (!file.exists(filename)) 
    stop(str_c("File ",filename," does not exist."))
  bid = readLines(filename)
  header = str_trim(bid[1])
  if (header != "Test and Patient parameters")
    stop(str_c("File ",filename," is not a valid BreathID file."))
  Date = findSinglePattern(bid,"Date")
  EndTime = strptime(str_c(Date, " ",findSinglePattern(bid,"End time") ),
                     "%m/%d/%y %H:%M")
    
  StartTime =strptime(str_c(Date, " ",findSinglePattern(bid,"Start time") ),
                      "%m/%d/%y %H:%M")
  PatientNumber = findSinglePattern(bid,"Patient #")
  PatientID = findSinglePattern(bid,"Patient ID",FALSE)
  TestNo = as.integer(findSinglePattern(bid,"Test No."))
  Type = findSinglePattern(bid,"Type")
  Gender = findSinglePattern(bid,"Gender")
  if (nchar(Gender)> 0) 
    Gender = str_sub(tolower(Gender),1,1)
  Dose = as.numeric(findSinglePattern(bid,"Dose"))
  Height = as.numeric(findSinglePattern(bid,"Height"))
  Weight = as.numeric(findSinglePattern(bid,"Weight"))  
  T50 = as.numeric(findSinglePattern(bid,"T 1/2"))  
  TLag = as.numeric(findSinglePattern(bid,"T lag"))  
  GEC = as.numeric(findSinglePattern(bid,"GEC")) 
  bid =try(
    str_trim(bid[which(str_detect(bid,"Time\\s*DOB")):length(bid)]),silent=TRUE)
  if (class(bid)=="try-error")
    stop(str_c("File ",filename," does not contain PDR data"))
  bid = bid[bid!=""]
  if (length(bid)<2)
    stop(str_c("File ",filename," does not contain PDR data"))
  data = read.table(textConnection(bid),header=TRUE)
  data = RemoveNAColumns(data)
  structure(list(
       FileName=basename(filename),
       EndTime=EndTime,
       StartTime=StartTime,
       PatientNumber=PatientNumber,
       Name = as.character(NA),
       FirstName = as.character(NA),
       Initials = as.character(NA),
       TestNo=TestNo,
       Gender = Gender,
       Type=Type,
       Dose=Dose,
       Height=Height,
       Weight=Weight,
       T50=T50,
       TLag=TLag,
       GEC=GEC,
       Data=data),class="breathIdData")
      
}

RemoveNAColumns = function(x) {
  x[,!unlist(lapply(x,function(y) all(is.na(y)| is.nan(y)|str_trim(y)=="" )))]
} 

findSinglePattern = function(bid,pattern,required=TRUE){
  p = str_match(bid,str_c(pattern,"[^-]+-\\s*(\\S*)"))[,2]
  p = p[!is.na(p)]
  if (length(p)>1) 
    stop(str_c("No unique <<", pattern,">> in BreathID file"))
  if (length(p)==0)
    if (required)
      stop(str_c("No <<" ,pattern, ">> found in BreathID file "))
    else 
      p=""
  return(str_trim(p))
}





