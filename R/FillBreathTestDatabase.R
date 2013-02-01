#' @title Create and sample fill sqlite database for D13CBreath
#' @description 
#' These functions can be used for testing, or as a sample
#' how to write data to the database. Using sqlite allows for full control of
#' database and table creation from the script; creating a Microsoft Access database 
#' is not possible from RODBC, creating tables is possible, but
#' "ON UPDATE DELETE" fails and you will need to dig deep into webspace
#' to find it is not your fault. 
#'
#' @name CreateBreathTestDatabase
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param sqlitePath Full filename with path to create database file. 
#' The file will not be overwritten if it exists.
#' @examples
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateBreathTestDatabase(sqlitePath)
#' @export CreateBreathTestDatabase
CreateBreathTestDatabase = function(sqlitePath){
  if (file.exists(sqlitePath))
    stop(str_c("The database", basename(sqlitePath),
               " already exists, please delete it manually to proceed."))
  con = OpenSqliteConnection(sqlitePath)  
  createPatient = 
  'CREATE TABLE IF NOT EXISTS Patient (
    PatientID TEXT PRIMARY KEY  NOT NULL , 
    Name TEXT, 
    FirstName TEXT, 
    Initials TEXT, 
    DOB DATETIME, 
    BirthYear INTEGER, 
    Gender CHAR, 
    Study TEXT, 
    PatStudyID TEXT)'
  
  createBreathTestRecord = '
  CREATE TABLE IF NOT EXISTS BreathTestRecord(
    BreathTestRecordID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    FileName TEXT NOT NULL UNIQUE,
    Device TEXT,
    PatientID TEXT NOT NULL,
    RecordDate DateTime,
    StartTime DateTime,
    EndTime DateTime,
    TestNo INTEGER,
    Dose REAL,
    Height REAL,
    Weight REAL,
    FOREIGN KEY (PatientID) REFERENCES Patient(PatientID) ON DELETE CASCADE
    )'
  
  createBreathTestTimeSeries = '
  CREATE TABLE IF NOT EXISTS BreathTestTimeSeries(
    BreathTestTimeSeriesID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    BreathTestRecordID INTEGER NOT NULL,
    Time INTEGER NOT NULL, -- In minutes after start
    Parameter TEXT NOT NULL, -- cDOB, DOB, PDR,cPDR
    Value REAL NOT NULL,
    CONSTRAINT unq UNIQUE (BreathTestRecordID, Time, Parameter),
    FOREIGN KEY (BreathTestRecordID) 
       REFERENCES BreathTestRecord(BreathTestRecordID) ON DELETE CASCADE
  )'
  
  createBreathTestParameter = '
  CREATE TABLE IF NOT EXISTS BreathTestParameter(
    BreathTestParameterID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    BreathTestRecordID INTEGER NOT NULL,
    Parameter TEXT NOT NULL, -- t50, tlag, GEC, k, m, beta
    Method TEXT NOT NULL, -- How the value was computed: bluckSC, ghoos, BreathID, ghoosScinti
    Value REAL NOT NULL,
    FOREIGN KEY (BreathTestRecordID) 
       REFERENCES BreathTestRecord(BreathTestRecordID) ON DELETE CASCADE
  )'
  
#  dbSendQuery(con,"DROP TABLE IF EXISTS Patient")
#  dbSendQuery(con,"DROP TABLE IF EXISTS BreathTestRecord")
#  dbSendQuery(con,"DROP TABLE IF EXISTS BreathTestTimeSeries")
#  dbSendQuery(con,"DROP TABLE IF EXISTS BreathTestParameter")
  dbSendQuery(con,createPatient)
  dbSendQuery(con,createBreathTestRecord)
  dbSendQuery(con,createBreathTestTimeSeries)
  res = dbSendQuery(con,createBreathTestParameter)
  dbClearResult(res)
  dbDisconnect(con)
  return (invisible(NULL))
}  


#' @title Opens sqlite database connection
#' @name OpenSqliteConnection
#' @description Opens an connection to sqlite database; creates the database
#' if it does not exists
#' @param sqlitePath Full filename with path to create database file. 
#' @return con Connection for use with dbSendQuery and dbGetQuery
#' @export
OpenSqliteConnection = function(sqlitePath=NULL){
  if (is.null(sqlitePath))
    sqlitePath = file.path(Sys.getenv("HOME"),
                           "Gastrobase2/Gastrobase2.sqlite")
  if (!file.exists(sqlitePath)){
    # Create Path
    path = dirname(sqlitePath)
    if (!file.exists(path)) dir.create(path)
  }
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = sqlitePath)
  dbSendQuery(con,"PRAGMA foreign_keys=ON")
  if (dbGetQuery(con,"PRAGMA foreign_keys") != 1)
    stop("This version of sqlite does not support foreign key constraints")
  return (con)
}

#' @title Creates a simulated BreathID record for testing
#' @name SimulateBreathId
#' @return A record that is a modified version of the provided sample
#' record, with additional fields \code{Name, FirstName, Initials, FileName}
#' @seealso \code{\link{ReadBreathId}}
#' @export
SimulateBreathId = function(){
  # To be sure we get the format right, we read one sample from BreathID
  filename = system.file("extdata", "350_20043_0_GER.txt", 
                         package = "D13CBreath")
  bid = ReadBreathId(filename)
  bid$FirstName = RandomName()
  bid$Name = RandomName()
  bid$Initials = str_c(str_sub(bid$FirstName,1,1),
                       str_sub(bid$Name,1,1))
  bid$FileName = RandomFile()
  bid$StartTime = Sys.time() -  60*60*24*200*runif(1)
  bid$EndTime = bid$StartTime+ rnorm(1,60*100,20)
  bid$PatientNumber = sample(c("Alpha","Beta","Gamma","Delta"),1)
  bid$TestNo = sample(20000:30000,1)
  bid$Height = rnorm(1,180,5)
  bid$Weight = rnorm(1,70,5)
  
  start = list(m=20,k=1/100,beta=2)
  m = rnorm(1,start$m,start$m*0.1)
  k = rnorm(1,start$k,start$k*0.1)
  beta = rnorm(1,start$beta,start$beta*0.05)
  
  bid$Data$DOB =as.numeric(bluckCoward(bid$Data$Time,100,m,k,beta))+
                     rnorm(length(bid$Data$Time),0,0.5)
  bid$Data$PDR = bid$Data$DOB* rnorm(1,1.2,0.1)
  bid$Data$PDRfit= NULL
  bid$Data$CPDR= NULL
  bid$Data$CPDRfit= NULL
  bid
}

#' @title Adds a simulated breath test record to the database
#' @name AddSimulatedBreathTestRecord
#' @description Creates a simulated data record, computes several fit 
#' parameters, and append these to the database 
#' @param con Connection to sqlite database
#' @examples
#' if (exists("con")) suppressWarnings(dbDisconnect(con))
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateBreathTestDatabase(sqlitePath)
#' con = OpenSqliteConnection()
#' add = try (
#'   for (i in 1:10)
#'     AddSimulatedBreathTestRecord(con),silent = TRUE)
#' if (inherits(add,"try-error")) dbRollback(con) else dbCommit(con)
#' dbDisconnect(con)
#' @export
AddSimulatedBreathTestRecord = function(con){
  bid = SimulateBreathId()
  # Check if patient exists
  PatientID = bid$PatientNumber
  q = sprintf("SELECT COUNT(*) from Patient where PatientID='%s'",
              bid$PatientNumber)
  if (dbGetQuery(con,q) == 0) 
  {
    # Must insert Patient
    q = with(bid,sprintf("INSERT INTO Patient (PatientID,Name,FirstName,Initials)
        VALUES ('%s','%s','%s','%s')",
        PatientID,Name,FirstName,Initials))
    tryCatch( dbSendQuery(con,q), 
     error=function(e) stop(str_c("Error inserting PatientID",PatientID)))
  }
  q = with(bid,sprintf("INSERT INTO BreathTestRecord (Filename, Device,
    PatientID,RecordDate,StartTime,EndTime,TestNo,Dose) VALUES (
    '%s','%s','%s','%s','%s','%s',%d,%d)",
     FileName, "BreathID", PatientNumber,StartTime,StartTime,EndTime,TestNo,Dose))
  ret = try(dbGetQuery(con,q),TRUE)
  if (inherits(ret,"try-error"))
  {
    if (str_detect(ret,"unique")){  
      stop(str_c("A record for file ",bid$FileName," already exists. Skipped."))
    } else {
      stop(ret)
    }
  }
  BreathTestRecordID = LastInsertRowid(con)
  bts = melt(bid$Data,"Time",variable.name="Parameter",value.name="Value")
  bts$BreathTestRecordID = BreathTestRecordID
  bts$BreathTestTimeSeriesID = NA
  # Retrieve column names to get the order right, skipping autoincrement
  flds = dbListFields(con,"BreathTestTimeSeries")
  success = dbWriteTable(con,"BreathTestTimeSeries",bts[,flds],append=TRUE,
                         row.names=FALSE)
  if (!success)
    stop(str_c("Could not write raw time series record for patient",PatientID))
  start = list(m=20,k=1/100,beta=2)
  Dose = bid$Dose
  # Fit Model and compute prediction
  bid.nls = nls(PDR~bluckCoward(Time,Dose,m,k,beta),
                data=bid$Data[bid$Data$Time > 0,], start=start)
  cf = coef(bid.nls)
  bidPred = data.frame(BreathTestTimeSeriesID=NA,
    BreathTestRecordID,
    Time = seq(min(bid$Data$Time),max(bid$Data$Time)+5,by=5))
  bidPred$Parameter="PDRFitBC"
  bidPred$Value = predict(bid.nls,newdata=bidPred)
  # Save Prediction
  success = dbWriteTable(con,"BreathTestTimeSeries",bidPred,append=TRUE,
                         row.names=FALSE)
  if (!success)
    stop(str_c("Could not write fitted time series record for patient",
               PatientID))
  # Write parameters
  pars = data.frame(BreathTestParameterID=as.integer(NA), BreathTestRecordID,
    Parameter = c("t50","t50","t50","t50","tlag","tlag","tlag","GEC"),
    Method = c("BreathID","BluckCoward","Ghoos","GhoosScint",
               "BreathID","BluckCoward","Ghoos","BreathID"),
    Values = c(bid$T50,t50BluckCoward2(cf),t50Ghoos(cf),t50GhoosScintigraphy(cf),
               bid$TLag,tLagBluckCoward(cf),tLagGhoos(cf),bid$GEC)
    )
  success = dbWriteTable(con,"BreathTestParameter",pars,append=TRUE,
                         row.names=FALSE)
  if (!success)
    stop(str_c("Could not write parameters for patient",PatientID))
}

RandomName = function(nLetters=6){
  paste(sample(LETTERS[1:26],1,TRUE),
        paste(sample(letters[1:26],nLetters-1,TRUE),collapse=""),sep="")
}
RandomFile = function(ext="txt"){
  paste(paste(sample(100:999,1,TRUE),sample(10000:99999,1,TRUE),
              sample(1:9,1),sep="_"),".",ext,sep="")
}  

LastInsertRowid = function(con){
  as.integer(dbGetQuery(con,"SELECT last_insert_rowid()")[1,1])
}

