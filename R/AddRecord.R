#' @name AddRecord
#' @title Reads BreathID or IRIS records and writes parameters 
#' to database
#' @description
#' Reads BreathID or Iris data record, computes several fit
#' parameters and a fit, and writes these to the database.
#'
#' @param filename Name of BreathID/Iris file
#' @param con connection to sqlite database, e.g. from \code{OpenSqliteConnection}
#' @examples
#' sqliteFile = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqliteFile)
#' sqlitePath = CreateEmptyBreathTestDatabase(sqliteFile)
#' con = OpenSqliteConnection(sqliteFile)
#' filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
#' AddBreathTestRecord(filename,con)
#' filename = system.file("extdata", "IrisCSV.TXT", package = "D13CBreath")
#' AddIrisCSVRecord(filename,con)
#' filename = system.file("extdata", "IrisMulti.TXT", package = "D13CBreath")
#' AddIrisRecord(filename,con)
#' dbDisconnect(con)
#' @rdname AddRecord
#' @export

AddBreathTestRecord = function(filename,con) {
  bid = ReadBreathId(filename)
  BreathTestRecordToDatabase(bid,con)
}

#' @rdname AddRecord
#' @export
AddIrisRecord = function(filename,con) {
  bid = ReadIris(filename)
  BreathTestRecordToDatabase(bid,con)
}


#' @rdname AddRecord
#' @export
AddIrisCSVRecord = function(filename,con) {
  bid = ReadIrisCSV(filename)
  BreathTestRecordToDatabase(bid,con)
}
