#' @title Reads 13C data from database, computes parameterized fit parameters 
#' @description 
#' Reads record with given BreathTestRecordID from database and computes 
#' ExpBeta fit parameter which are written back to table BreathTestParameter
#' Existing parameters are overwritten. 
#' @name ComputeAndSaveParameterizedFit
#' @examples
#' sqliteFile = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(sqliteFile)
#' BreathTestRecordID = 1
#' ComputeAndSaveParameterizedFit(con,BreathTestRecordID)
#' dbDisconnect(con)
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param con connection to SQlite database
#' @param BreathTestRecordID these data will be read from BreathTestTimeSeries
#' @export 
ComputeAndSaveParameterizedFit = function(con,BreathTestRecordID)  {
  start = list(m=20,k=1/100,beta=2)
  Dose = try(dbGetQuery(con, paste(
    "SELECT Dose from BreathTestRecord where BreathTestRecordID=",
    BreathTestRecordID),  sep="")[1,1])
  if (inherits("Dose","try-error")) Dose=100
  data = dbGetQuery(con,paste(
    "SELECT Time, Value as PDR from BreathTestTimeSeries where BreathTestRecordID=" ,
    BreathTestRecordID, " and Parameter = 'PDR' and Time > 0 order by Time",sep=""))
  if (inherits("data","try-error")) 
    stop(paste("No data found for BreathTestRecordID",BreathTestRecordID))
  # Fit Model and compute prediction
  bid.nls = try(suppressWarnings(nls(PDR~ExpBeta(Time,Dose,m,k,beta),
        data=data, start=start)),silent=TRUE)
  if (inherits(bid.nls,"try-error"))
    return(NULL) # Skip this
  cf = coef(bid.nls)
  # Delete old values
  methods = c("ExpBeta","ExpBeta","ExpBeta","BluckCoward","Maes","MaesScint",
    "BluckCoward","Maes")
  dbSendQuery(con, paste(
      "DELETE FROM BreathTestParameter where BreathTestRecordID=",
      BreathTestRecordID ," and Method in ('",
      paste(unique(methods),collapse="','",sep=""),"')",sep=""))
  # Write parameters and coefficients
  pars = data.frame(BreathTestParameterID=as.integer(NA), BreathTestRecordID,
                    Parameter = c("m","k","beta","t50","t50","t50","tlag","tlag"),
                    Method = methods,
                    Value = unlist(c(cf["m"],cf["k"],cf["beta"],
                                     t50BluckCoward(cf),
                                     t50Maes(cf),t50MaesScintigraphy(cf),
                                     t50BluckCoward(cf),tLagMaes(cf)))
  )
  
  q = str_c("INSERT INTO BreathTestParameter VALUES(",
            paste(rep("?",ncol(pars)),collapse=","),")")
  ret = try(dbGetPreparedQuery(con, q,bind.data= pars), silent=TRUE)
  if (inherits(ret,"try-error"))
    stop(str_c("Could not write fit parameters BreathTestRecordID ",
               BreathTestRecordID))
  
}
