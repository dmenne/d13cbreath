#' @title Create an empty SQLite database for breath test data
#' @description
#' These functions can be used for testing, or as a sample
#' how to write data to the database. Using sqlite allows for full 
#' control of database and table creation from the script.
#'
#' @name CreateEmptyBreathTestDatabase
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param sqlitePath Full filename with path to create database file.
#' The file will not be overwritten if it exists.
#' Use \code{getOption("Gastrobase2SqlitePath")} to find the default path for the database.
#' @examples
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateEmptyBreathTestDatabase(sqlitePath)
#' \dontrun{
#' # This creates a default database, but does not overwrite existing files
#' CreateEmptyBreathTestDatabase(getOption("Gastrobase2SqlitePath"))
#' }
#' @importFrom DBI dbExecute
#' @export
CreateEmptyBreathTestDatabase = function(sqlitePath) {
  if (file.exists(sqlitePath))
    stop(
      paste0(
        "The database", basename(sqlitePath),
        " already exists, please delete it manually to proceed."
      )
    )
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
  PatStudyID TEXT,
  Status INTEGER DEFAULT 0)'
  
  createBreathTestRecord = '
  CREATE TABLE IF NOT EXISTS BreathTestRecord(
  BreathTestRecordID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  FileName TEXT NOT NULL UNIQUE,
  Device TEXT,
  Substrate TEXT,
  PatientID TEXT NOT NULL,
  RecordDate DateTime,
  StartTime DateTime,
  EndTime DateTime,
  TestNo INTEGER,
  Dose REAL,
  Height REAL,
  Weight REAL,
  Status INTEGER DEFAULT 0,
  FOREIGN KEY (PatientID) REFERENCES Patient(PatientID) ON DELETE CASCADE ON UPDATE CASCADE
  )'

  createBreathTestTimeSeries = '
  CREATE TABLE IF NOT EXISTS BreathTestTimeSeries(
  BreathTestTimeSeriesID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  BreathTestRecordID INTEGER NOT NULL,
  Time REAL NOT NULL, -- In minutes after start
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
  Method TEXT NOT NULL, -- How the value was computed: bluckSC, Maes, BreathID, MaesScinti
  Value REAL NOT NULL,
  CONSTRAINT unq UNIQUE (BreathTestRecordID, Method, Parameter),
  FOREIGN KEY (BreathTestRecordID)
  REFERENCES BreathTestRecord(BreathTestRecordID) ON DELETE CASCADE
  )'

  createShowParameters =
    'CREATE TABLE IF NOT EXISTS "ShowParameters" (
  "Parameter" VARCHAR NOT NULL ,
  "Method" VARCHAR NOT NULL ,
  "Show" INT NOT NULL  DEFAULT 0,
  PRIMARY KEY ("Method", "Parameter"))'
  createSettings = 'CREATE TABLE IF NOT EXISTS "Setting" (
  "SettingID" CHAR PRIMARY KEY  NOT NULL ,
  "Value" CHAR)'
  index1 =
    'CREATE INDEX BreathTestRecordPatientID ON BreathTestRecord (PatientID)'
  index2 =
    'CREATE INDEX BreathTestParameterBreathTestRecordID ON BreathTestParameter (BreathTestRecordID)'
  index3 =
    'CREATE INDEX BreathTestTimeSeriesBreathTestRecordID  ON BreathTestTimeSeries (BreathTestRecordID)'
  
  #  dbExecute(con,"DROP TABLE IF EXISTS Patient")
  #  dbExecute(con,"DROP TABLE IF EXISTS BreathTestRecord")
  #  dbExecute(con,"DROP TABLE IF EXISTS BreathTestTimeSeries")
  #  dbExecute(con,"DROP TABLE IF EXISTS BreathTestParameter")
  dbExecute(con,createPatient)
  dbExecute(con,createBreathTestRecord)
  dbExecute(con,createBreathTestTimeSeries)
  dbExecute(con,createShowParameters)
  dbExecute(con,createSettings)
  dbExecute(con,createBreathTestParameter)
  dbExecute(con,index1)
  dbExecute(con,index2)
  dbExecute(con,index3)
  ## Avoid closing error
  dbDisconnect(con)
  return (invisible(NULL))
}

#' @title Opens sqlite database connection
#' @name OpenSqliteConnection
#' @description Opens an connection to sqlite database; creates the database
#' if it does not exists. If missing, file name is given by
#' \code{getOption("Gastrobase2SqlitePath")} which is set to
#' <HOME>/GastroBase/Gastobase/Gastrobase2.sqlite at package load time.
#'
#' When \code{options(D13CBreath.sqldebug=TRUE)}, SQL of queries is printed out
#' @param sqlitePath Full filename with path to create database file.
#' @return con Connection for use with dbExecute and dbGetQuery
#' @import stringr
#' @import reshape2
#' @examples
#' \dontrun{
#' con = OpenSqliteConnection()
#' dbGetQuery(con, "Select PatientID,Name,FirstName from Patient")
#' dbDisconnect(con)
#' }
#' @importFrom DBI dbExecute
#' @export
OpenSqliteConnection = function(sqlitePath = NULL) {
  if (is.null(sqlitePath))
    sqlitePath = getOption("Gastrobase2SqlitePath")
  if (!file.exists(sqlitePath)) {
    # Create Path
    path = dirname(sqlitePath)
    if (!file.exists(path))
      dir.create(path)
  }
  m <- RSQLite::SQLite()
  con <- dbConnect(m, dbname = sqlitePath)
  dbExecute(con,"PRAGMA foreign_keys=ON")
  if (dbGetQuery(con,"PRAGMA foreign_keys") != 1)
    stop("This version of sqlite does not support foreign key constraints")
  return (con)
}



#' @title Reads and saves multiple 13C Breath test records
#' @name AddAllBreathTestRecords
#' @description
#' Reads all BreathID and Iris/Wagner data records in a directory.
#' Computes several fit parameters and a fit, and writes these to the database.
#' Files that are already in the database are skipped. Note only the base name is tested,
#' so that files with
#' the same name in different directories are considered identical without testing.
#'
#' @param path start path for recursive search; can be a vector of
#' multiple start paths.
#' @param con connection to sqlite database
#' @return A dataframe with columns \code{file}, \code{basename},
#' \code{recordID} (NULL if not saved) and \code{status}
#' with levels \code{"saved", "skipped", "invalid"}.
#' @examples
#' if (exists("con")) suppressWarnings(dbDisconnect(con))
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateEmptyBreathTestDatabase(sqlitePath)
#' con = OpenSqliteConnection(sqlitePath)
#' path = dirname(
#'   system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath"))
#' AddAllBreathTestRecords(path,con)
#' dbDisconnect(con)
#'
# con = OpenSqliteConnection()
#path = c("C:/Users/Dieter/Documents/Gastrobase2/Iris")
#         "C:/Users/Dieter/Documents/Gastrobase2/BreathID")
#' @export
AddAllBreathTestRecords = function(path,con) {
  files = data.frame(
    file = dir(
      path,pattern = "*.txt",ignore.case = TRUE,
      recursive = TRUE,full.names = TRUE
    ),stringsAsFactors = FALSE
  )
  if (nrow(files) == 0)
    stop("No file found in path")
  files$basename = basename(files$file)
  files$recordID = NA
  files$status = NA
  files$error = ""
  files$device = NA
  # Check database for files already processed
  doneFiles = dbGetQuery(con,"SELECT filename from BreathTestRecord")[,1]
  if (length(doneFiles > 0)) {
    skipped = files$basename %in% doneFiles
    files$status[skipped] = "skipped"
    files$device[!skipped] = DeviceType(files$file[!skipped])
  } else
    files$device = DeviceType(files$file)
  # process all files
  for (i in seq(along = files$file)) {
    if (is.na(files[i,"device"]))
      # skip known
      next
    filename = files[i,"file"]
    device = files[i,"device"]
    if (device == "invalid") {
      files[i,"error"] = "Unrecognized device type"
      files[i,"status"] = "invalid"
      next
    } else {
      if (device == "BreathID") {
        bid = try(ReadBreathId(filename), silent = TRUE)
      } else if (device == "Iris")  {
        bid = try(ReadIris(filename), silent = TRUE)
      } else if (device == "IrisCSV")  {
        bid = try(ReadIrisCSV(filename), silent = TRUE)
      }
    if (inherits(bid,"try-error")) {
        files[i,"error"] = attr(bid,"condition")$message
        files[i,"status"] = "invalid"
        next
      }
    }
    recId = try(BreathTestRecordToDatabase(bid,con),silent = TRUE)
    if (inherits(recId,"try-error")) {
      files[i,"error"] = attr(recId,"condition")$message
      files[i,"status"] = "skipped"
      next
    }
    files[i,"recordID"] = recId
    files[i,"status"] = "saved"
  }
  files$status = as.factor(files$status)
  # Rearrange for easier printout
  files[,c(2,3,4,1,5)]
}

#' @title Guess device type of a text file
#' @name DeviceType
#' @description Reads the first line of the files, and returns
#' "BreathID","Iris", or "invalid"
#' @param files character vector of files
#' @return character vector of device types
#' @examples
#' path = dirname(
#'   system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath"))
#' files = dir(path,pattern="*.txt",ignore.case=TRUE,
#'         recursive=TRUE,full.names=TRUE)
#' DeviceType(files)
#' @export
DeviceType = function(files) {
  unlist(lapply(files, function(file) {
    line = str_trim(readLines(file,1))
    if (line == "Test and Patient parameters")
      return("BreathID")
    if (line == '"Testergebnis"')
      return("Iris")
    if (str_detect(line, "Identifikation"))
      return("IrisCSV")
    return("invalid")
  }))
}


#' @title Recompute all fit parameters
#' @name RebuildFitDatabase
#' @description Recomputes all fits parameters, excluding population fit. Use this
#' function to refresh coefficients when the algorithm has changed.
#' @param con Connection to sqlite database; if missing, default database in
#' path \code{getOption("Gastrobase2SqlitePath")} is used.
#'
#' @export
RebuildFitDatabase = function(con = NULL) {
  localCon = is.null(con)
  if (localCon)
    con = OpenSqliteConnection()
  rid = dbGetQuery(con,"SELECT BreathTestRecordID from BreathTestRecord")[,1]
  # Faster delete, this is optimized to TRUNCATE by SQLite
  dbExecute(con,"DELETE FROM BreathTestParameter")
  dbExecute(con,"DELETE FROM sqlite_sequence where name='BreathTestParameter'")
  lapply(rid,function(BreathTestRecordID) {
    ComputeAndSaveParameterizedFit(con,BreathTestRecordID)
    ComputeAndSaveWNFit(con,BreathTestRecordID) # This requires the parameterized fit
    invisible(NULL)
  })
  #RebuildPopulationFitDatabase(con)
  if (localCon){
    dbDisconnect(con)
  }
}

#' @title Computes fit and writes a 13C record and extracted parameters to databse
#' @name BreathTestRecordToDatabase
#' @description Appends measured values of a record to the database. Skips saving if the
#' file is already in the database. To overwrite an existing file,
#' the old record must be manually deleted from the database.
#' Computes and saves the extracted parameters from ExpBeta and Wagner-Nelson Fit.
#'
#' Table Patient:  Creates patient if required.
#'
#' Table BreathTestRecord: PatientID (refers to Patient); Filename, Device, ...
#'
#' Table BreathTestTimeSeries: Original times series as \code{Parameter=BreathID},
#'
#' @return BreathTestRecordID of added record, or NULL if not written.
#' @param bid Record as simulated by \code{SimulateBreathID} or \code{ReadBreathID}
#' @param con Connection to sqlite database
#' @export
BreathTestRecordToDatabase = function(bid, con) {
  # Nested transactions are not possible with dbBegin in SQlite,
  # therefore within the transaction it is not allowed to use dbWriteTable
  # which opens a transaction. Must use prepared queries instead.
  #
  ## TODO: The above comment is old, nesting is now possible, with named
  ## transactions, but I have not changed the code yet
  #
  if (is.null(bid))
    return(NULL)
  if (!inherits(bid, "BreathTestData"))
    stop("BreathTestRecordToDatabase: bid must of class 'BreathTestData'")
  # Wrap everything in a transaction
  dbBegin(con, name = "BreathTestRecordToDatabase")
  ## Do not use dbWriteTable in any nested function
  ret = try(BreathTestRecordToDatabaseInternal(bid,con), silent = TRUE)
  if (inherits(ret,"try-error")) {
    dbRollback(con, name = "BreathTestRecordToDatabase")
    stop(attr(ret,"condition")$message)
  }
  dbCommit(con, name = "BreathTestRecordToDatabase")
  ret
}

#' @title Saves Breath Test Parameters to Database
#' @name SaveBreathTestParameters
#' @description Mainly for internal use, but can be called to store any
#' additional key/value pair
#' @param con Connection to sqlite database
#' @param pars Data frame with columns BreathTestRecordID, Parameter, Method, Value
#' @export
SaveBreathTestParameters = function(con, pars){
  ret = NULL
  if (nrow(pars) > 0) {
    pars = cbind(BreathTestParameterID = as.integer(NA), pars)
    facs = sapply(pars, is.factor)
    pars[facs] = lapply(pars[facs], as.character)
    ret = try(
      dbExecute(con,
      "INSERT INTO BreathTestParameter VALUES($BreathTestParameterID, 
      $BreathTestRecordID, $Parameter, $Method, $Value)",  params = pars),
      silent =  TRUE
    )
  }
  ret
}



## This internal function does the work, and is wrapped by try in the exported
## function BreatTestRecordToDatabase
BreathTestRecordToDatabaseInternal = function(bid, con) {
  BreathTestRecordID = SavePatientRecord(bid,con)
  # Device specific (not always present)
  pars = na.omit(data.frame(
    BreathTestRecordID,
    Parameter = c("t50","tlag","GEC"),
    Method = rep(bid$Device,3),
    Value = c(bid$T50, bid$TLag,bid$GEC)
  ))
  ret = try(
    SaveBreathTestParameters(con, pars),
    silent = TRUE
  )
  if (inherits(ret, "try-error"))
    stop(paste0("Error writing device parameters for patient ",bid$PatientID))
  
  # Compute and save fit (will do nothing if not successful)
  ComputeAndSaveParameterizedFit(con,BreathTestRecordID)
  # WN requires the parameterized fit. In case it fails, 
  # no WN is written, but no error is raised
  ComputeAndSaveWNFit(con,BreathTestRecordID) 
  BreathTestRecordID
}

sn = function(x) {
  ifelse(is.null(x) ||
         is.na(x),"NULL",paste0("'",as.character(x),"'"))
}

SavePatientRecord = function(bid,con) {
  # returns last inserted RecordID
  # Check if patient exists
  printSQL = unlist(options("D13CBreath.sqldebug"))
  PatientID = bid$PatientID
  q = sprintf("SELECT COUNT(*) from Patient where PatientID='%s'",
              bid$PatientID)
  if (printSQL)
    print(q)
  if (dbGetQuery(con,q) == 0)
  {
    # Must insert Patient
    q = with(
      bid,sprintf(
        "INSERT INTO Patient
        (PatientID,Name,FirstName,Initials,DOB,BirthYear,Gender,Study,PatStudyID)
        VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s)",
        sn(PatientID),sn(Name),sn(FirstName),sn(Initials),sn(DOB),sn(BirthYear),
        sn(Gender),sn(Study),sn(PatStudyID)
      )
    )
    if (printSQL)
      print(q)
    # Make sure to use utf8 here for umlauts in names
    q = enc2utf8(q)
    tryCatch(
      dbExecute(con,q),
      error = function(e)
        stop(paste0(
          "Error inserting PatientID ",PatientID
        ))
    )
  }
  q = with(
    bid,sprintf(
      "INSERT INTO BreathTestRecord (Filename, Device,Substrate,
      PatientID,RecordDate,StartTime,EndTime,TestNo,Dose,Height,Weight,Status) VALUES (
      %s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
      sn(FileName), sn(Device), sn(Substrate),sn(PatientID),sn(RecordDate),
      sn(StartTime),sn(EndTime), sn(TestNo), sn(Dose), sn(Height), sn(Weight),0
  )
    )
  if (printSQL)
    print(q)
  ret = try(dbExecute(con,q),TRUE)
  if (inherits(ret,"try-error"))
  {
    if (str_detect(ret,"unique")) {
      stop(paste0(
        "A record for file ",bid$FileName," already exists. Skipped."
      ))
    } else {
      stop(attr(ret,"condition")$message)
    }
  }
  BreathTestRecordID = LastInsertRowid(con)
  bts = melt(bid$Data,"Time",variable.name = "Parameter",value.name = "Value")
  # Remove NA and NaN
  bts = bts[!(is.nan(bts$Value) | is.na(bts$Value)),]
  bts$BreathTestRecordID = BreathTestRecordID
  bts$BreathTestTimeSeriesID = NA
  # More recent versions of SQLite don't like factors
  facs = sapply(bts, is.factor)
  bts[facs] = lapply(bts[facs], as.character)
  # Retrieve column names to get the order right
  flds = dbListFields(con,"BreathTestTimeSeries")
  
  q = paste0("INSERT INTO BreathTestTimeSeries VALUES($",
             paste0(flds, collapse = ",$"),")")
  if (printSQL)
    print(q)
  ret = try(
    dbExecute(con, q, params = bts),
    silent =  TRUE
  )
  
  if (inherits(ret,"try-error")){
    stop(paste0(
        "Could not write raw time series record for patient ",PatientID,"\n")
    )
  }
  BreathTestRecordID
}

LastInsertRowid = function(con) {
  as.integer(dbGetQuery(con,"SELECT last_insert_rowid()")[1,1])
}
