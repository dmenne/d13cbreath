#' @title Data structure for saving into database
#' @description Generates structure of class BreathTestData with required fields
#' and optional fields as an intermediate between file data and the SQlite database. 
#' All optional fields by default are NA
#' @param PatientID required, string or number for unique identification in database
#' @param Name optional
#' @param FirstName optional 
#' @param Initials optional, 2 characters, 1 number 
#' @param DOB optional Date of birth (not to be confused with "delta over baseline)
#' @param BirthYear optional
#' @param Gender optional m or f
#' @param Study optional name of study; can be used in population fit 
#' @param PatStudyID optional; Patient number within study. Does not need to be globally unique
#' @param FileName required; file where data were read from, or other unique string.
#' When data are read again, this string is tested and record is skipped when
#' same filename is already in database, therefore uniqueness is important. When some
#' record does not turn up in database after repeated reading, check if a record with
#' the same file name is already there, and rename the file to avoid collisions.
#' @param Device BreathID or Iris; default "generic"
#' @param Substrate Should contain string "ace" or "oct" or "okt", case insensitive. Will
#' be replaced by "acetate" or "octanoate"
#' @param RecordDate Required record date.
#' @param StartTime optional
#' @param EndTime optional
#' @param TestNo required; unique test number
#' @param Dose optional, default 100 mg
#' @param Height optional, in cm; when PDR must be calculated, default values are 
#' used; see \code{\link{DOBToPDR}}
#' @param Weight optional, in kg
#' @param T50  optional, only present if device computes this value
#' @param GEC  optional, only present if device computes this value
#' @param TLag optional, only present if device computes this value
#' @param Data data frame with at least 5 rows and columns \code{Time} and one 
#' or both of \code{DOB} or \code{PDR}. If PDR is missing, and Height, Weight and Substrate
#' are given, computes PDR via function DOBToPDR
#' @export
BreathTestData = function(
  PatientID, Name=NA, FirstName=NA, 
  Initials=NA, DOB=NA, BirthYear=NA,
  Gender=NA, Study=NA, PatStudyID=NA,  
  FileName, Device = "generic", Substrate, RecordDate, StartTime=RecordDate,
  EndTime = RecordDate, TestNo ,Dose=100, Height=NA, Weight=NA, 
  T50 = NA, GEC = NA, TLag=NA, # Only if already stored in file, e.g BreathID
  Data=Data){
  
  if (!inherits(Data,"data.frame")) 
    stop("Function BreathTestData: Data must be a data frame.")
  if (nrow(Data)<5) 
    stop("Function BreathTestData: Data should have a least 5 rows.")
  nd = names(Data)
  if (nd[1] != "Time")
    stop("Function BreathTestData: First data column must be Time")
  if (!sum(nd[-1] %in% c("PDR","DOB")) >0) 
    stop("Function BreathTestData: Data should have either DOB or PDR or both")
  ##### Add more substrates here
  substrates = c("octanoate","acetate")
  substratePattern = c("o[ck]t","acet")
  substrate = substrates[str_detect(tolower(Substrate),substratePattern)][1]
  if (length(substrate)==0)
    stop("Function BreathTestData: Substrate is '", Substrate,
         "'; it should contain substrings '" ,paste(str_sub(substrates,1,4),collapse="' or '"),"'")
  if (!is.na(Gender) & ! match(Gender,c("m","f")))
    stop("Function BreathTestData: Gender should be 'm' or 'f'")
  if (! "PDR" %in% nd )  
    Data$PDR = DOBToPDR(Data$DOB,Weight,Height,MW=substrate)
  structure(list(
    PatientID = PatientID, Name=Name, FirstName=FirstName, Initials=Initials, 
    DOB=DOB, BirthYear=BirthYear,
    Gender=Gender, Study=Study, PatStudyID=PatStudyID,  
    FileName=FileName, Device = Device, Substrate= substrate, 
    RecordDate=as.character(RecordDate), StartTime=as.character(StartTime),
    EndTime = as.character(EndTime), TestNo=TestNo ,Dose=100, Height=Height, Weight=Weight, 
    T50 = T50, GEC = GEC, TLag=TLag, 
    Data=Data),class="BreathTestData")
}

