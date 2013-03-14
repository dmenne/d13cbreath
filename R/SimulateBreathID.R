#' @title Creates a simulated BreathID record for testing
#' @name SimulateBreathId
#' @return A record that is a modified version of the provided sample
#' record, with additional fields \code{Name, FirstName, Initials, FileName}
#' @seealso \code{\link{ReadBreathId}}, \code{\link{AddSimulatedBreathTestRecord}}
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
  bid$Device = "BreathID"
  start = list(m=20,k=1/100,beta=2)
  m = rnorm(1,start$m,start$m*0.1)
  k = rnorm(1,start$k,start$k*0.1)
  beta = rnorm(1,start$beta,start$beta*0.05)
  
  bid$Data$DOB =as.numeric(BluckCoward(bid$Data$Time,100,m,k,beta))+
    rnorm(length(bid$Data$Time),0,0.5)
  bid$Data$PDR = bid$Data$DOB* rnorm(1,1.2,0.1)
  bid$Data$PDRfit= NULL
  bid$Data$CPDR= NULL
  bid$Data$CPDRfit= NULL
  bid
}


#' @title Adds one simulated breath test record to the database
#' @name AddSimulatedBreathTestRecord
#' @description Creates a simulated data record, computes several fit 
#' parameters, and append these to the database 
#' @param con Connection to sqlite database
#' @seealso \code{\link{ReadBreathId}}, \code{\link{SimulateBreathId}}, 
#' \code{\link{CreateSimulatedBreathTestDatabase}}
#' 
#' @examples
#' # This example does the same as function CreateSimulatedBreathTestDatabase
#' if (exists("con")) suppressWarnings(dbDisconnect(con))
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateEmptyBreathTestDatabase(sqlitePath)
#' con = OpenSqliteConnection(sqlitePath)
#' add = try (
#'   for (i in 1:10)
#'     AddSimulatedBreathTestRecord(con),silent = TRUE)
#' if (inherits(add,"try-error")) dbRollback(con) else dbCommit(con)
#' dbDisconnect(con)
#' @export
AddSimulatedBreathTestRecord = function(con){
  bid = SimulateBreathId()
  BreathTestRecordToDatabase(bid,con)
}


#' @title Creates a database with simulated breath tests
#' @name CreateSimulatedBreathTestDatabase
#' @description Creates a database withsimulated breath test records. 
#' If sqLitePath is NULL, it is created in temporary directory.
#' To create a file in the default path, use
#' \code{CreateSimulatedBreathTestDatabase(getOption("Gastrobase2SqlitePath"))}
#' @param sqlitePath Path to created sqlite file.
#' @return File name of simulated database
#' @export
CreateSimulatedBreathTestDatabase = function(sqlitePath=NULL){
  set.seed(4711)
  if (is.null(sqlitePath))
    sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  add = try (
    for (i in 1:10)
      AddSimulatedBreathTestRecord(con),silent = TRUE)
  if (inherits(add,"try-error")) dbRollback(con) else dbCommit(con)
  dbDisconnect(con)
  sqlitePath
}