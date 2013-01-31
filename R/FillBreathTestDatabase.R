#' @title Manages sqlite database connections
#' @description This function can be used for testing, or as a sample
#' how to write data to the database.
#' @name CreateBreathTestDatabase
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param path Full filename with path to create database file
#' @examples

library(RSQLite)
sqlitePath = file.path(Sys.getenv("HOME"),"Gastrobase2/Gastrobase2.sqlite")

path = dirname(sqlitePath)

if (!file.exists(path)) dir.create(path)
m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = sqlitePath)

createPat = 
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
  Device TEXT,
  PatientID TEXT NOT NULL,
  RecordDate DateTime,
  StartTime DateTime,
  EndTime DateTime,
  FOREIGN KEY (PatientID) REFERENCES Patient(PatientID) ON DELETE CASCADE
  )'


dbSendQuery(con,"DROP TABLE PATIENT")
dbSendQuery(con,"DROP TABLE BreathTestRecord")
dbSendQuery(con,createPat)
dbSendQuery(con,createBreathTestRecord)
dbSendQuery(con,"INSERT INTO Patient (PatientID) VALUES ('ASDF') ")
dbSendQuery(con,"INSERT INTO BreathTestRecord (PatientID) VALUES ('ASDF')")
rowID = dbGetQuery(con,"SELECT last_insert_rowid()")


# Check if there are data in the Patient table
sqlQuery(channel,"DELETE FROM Patient") ### Only for debugging !!!!
nPat = sqlQuery(channel, "SELECT COUNT(*) from Patient")[1,1]
if (nPat!=0)
  stop("Please manually delete all entries in the Patient table")
Patient = data.frame(PatientID=c("alpha","beta","gamma"),
                     Name= c("Alpha","Beta","Gamma"),
                     FirstName = c("al","bet","ga"),
                     Initials = c("AA1","BB1","CC1"),
                     DOB = NA,
                     BirthYear = c(1973:1975),
                     Gender = "m",
                     Study = "GastroStudy",
                     PatStudyID =c(NA,"BetG","GamG") )
sqlSave(channel, Patient,append=TRUE,rownames=FALSE)
nRecords = 10
StartTime = Sys.time()+3600*24*runif(nRecords)

BreathTestRecordA = data.frame(
     Device = "BreathID",
     PatientID = sample(Patient$PatientID,nRecords,replace=TRUE)
  )
sqlSave(channel,BreathTestRecordA,append=TRUE,rownames=FALSE,
        verbose=TRUE,fast=FALSE)

