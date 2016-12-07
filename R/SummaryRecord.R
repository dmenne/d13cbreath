#' @title Summary data from a 13C Record
#'
#' @description Reads record from database and returns extracted parameters
#'
#' @param con Open connection to SQLite database; use \code{OpenSqliteConnection}
#' to connect.
#'
#' @param breathTestRecordID BreathTestRecordID in database; used in tables
#' @return List with named vector \code{Record} (Patient and Record) and
#' data frame \code{Parameters}
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' sqlitePath = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(sqlitePath)
#' breathTestRecordID=1
#' Summary13CRecord(con,breathTestRecordID)
#' dbDisconnect(con)
#' @import ggplot2
#' @import RSQLite
#' @import RColorBrewer
#' @export
Summary13CRecord = function(con, breathTestRecordID) {
  q = paste0(
    "SELECT Parameter, Method, Value from BreathTestParameter where BreathTestRecordID = ",
    breathTestRecordID," ORDER BY Parameter, Method"
  )
  parm = dbGetQuery(con,q)
  if (nrow(parm) == 0)
    stop(paste0(
      "No parameters found for BreathTestRecordID ",breathTestRecordID
    ))
  
  q = paste0(
    "SELECT * from BreathTestRecord
    join Patient on Patient.PatientID = BreathTestRecord.PatientID
    where BreathTestRecordID = ",    breathTestRecordID
  )
  rec = dbGetQuery(con,q)
  
  list(Record = rec[1,],Parameters = parm)
}
