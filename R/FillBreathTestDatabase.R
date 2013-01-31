#' @title Manages sqlite database connections
#' @description This function can be used for testing, or as a sample
#' how to write data to the database.
#' @name CreateBreathTestDatabase
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param path Full filename with path to create database file
#' @examples

library(RODBC)
accessFile = file.path(Sys.getenv("HOME"),"Gastrobase2/Gastrobase2.accdb")
if (!file.exists(accessFile)) 
  stop(str_c(accessFile," not found"))
channel = odbcConnectAccess2007(accessFile)

DOB DATETIME, 

createPat = 
'CREATE TABLE Pat (
  PatientID varchar(20) PRIMARY KEY  NOT NULL , 
  Name varchar(30), 
  FirstName varchar(20), 
  Initials varchar(3), 
  DOB DATETIME,
  BirthYear INT, 
  Gender char(1), 
  Study varchar(10), 
  PatStudyID varchar(10))'

sqlQuery(channel,createPat)


createBreathTestRecordA = '
CREATE TABLE BreathTestRecordA(
  BreathTestRecordID AUTOINCREMENT PRIMARY KEY AUTOINCREMENT NOT NULL,
  Device varchar(10),
  PatientID varchar(20),
  RecordDate DateTime,
  StartTime DateTime,
  EndTime DateTime,
  CONSTRAINT FKPatientID FOREIGN KEY (PatientID) 
  REFERENCES Pat(PatientID) ON DELETE CASCADE)
'

sqlQuery(channel, "DROP TABLE BreathTestRecordA")
sqlQuery(channel,createBreathTestRecordA)



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

