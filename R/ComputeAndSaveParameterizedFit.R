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
#' @export ComputeAndSaveParameterizedFit
ComputeAndSaveParameterizedFit = function(con,BreathTestRecordID)  {
  Dose = try(dbGetQuery(con, paste(
    "SELECT Dose from BreathTestRecord where BreathTestRecordID=",
    BreathTestRecordID),  sep="")[1,1])
  if (inherits("Dose","try-error")) Dose=100
  data = dbGetQuery(con,paste(
    "SELECT Time, Value as PDR from BreathTestTimeSeries where BreathTestRecordID=" ,
    BreathTestRecordID, " and Parameter = 'PDR' and Time > 0 order by Time",sep=""))
  if (inherits("data","try-error")  | nrow(data)==0 | all(data$PDR == 0) ) 
    stop(paste("No PDR data found for BreathTestRecordID; did you supply weight and height?",BreathTestRecordID))
  # Fit Model and compute prediction
  # TODO: Shift values at t=0 to values a t=0.001 to avoid errors
  # TODO: Kill if values t<0 are present
  start = list(m=sum(data$PDR*diff(c(0,data$Time)))/70, # Empirical
               k=1/100,beta=2)
  bid.nls = try(suppressWarnings(nls(PDR~ExpBeta(Time,Dose,m,k,beta),
        data=data, start=start)),silent=TRUE)
  if (inherits(bid.nls,"try-error")){
    start = list(m=50,k=1/100,beta=1) # Try different start values
    bid.nls = try(suppressWarnings(nls(PDR~ExpBeta(Time,Dose,m,k,beta),
                                     data=data, start=start)),silent=TRUE)
  }
  if (inherits(bid.nls,"try-error"))
    return(NULL) # Skip this
  cf = coef(bid.nls)
  deviance = deviance(bid.nls)
  # Delete old values
  methods = c("ExpBeta","ExpBeta","ExpBeta","ExpBeta","BluckCoward","Maes","MaesScint",
    "BluckCoward","Maes")
  parameters = c("m","k","beta","deviance","t50","t50","t50","tlag","tlag")
  dbSendQuery(con, paste(
      "DELETE FROM BreathTestParameter where BreathTestRecordID=",
      BreathTestRecordID ," and Method in ('",
      paste(unique(methods),collapse="','",sep=""),"')",sep=""))
  # Write parameters and coefficients
  pars = data.frame(BreathTestParameterID=as.integer(NA), BreathTestRecordID,
                    Parameter = parameters,
                    Method = methods,
                    Value = unlist(c(cf["m"],cf["k"],cf["beta"],deviance,
                                     t50BluckCoward(cf),
                                     t50Maes(cf),t50MaesScintigraphy(cf),
                                     tLagBluckCoward(cf),tLagMaes(cf)))
  )
  # Dirty trick: remove zero t50BluckCowards
  pars = pars[pars$Value !=0,]
  q = str_c("INSERT INTO BreathTestParameter VALUES(",
            paste(rep("?",ncol(pars)),collapse=","),")")
  ret = try(dbGetPreparedQuery(con, q,bind.data= pars), silent=TRUE)
  if (inherits(ret,"try-error"))
    stop(str_c("Could not write fit parameters for record with BreathTestRecordID= ",
               BreathTestRecordID))
  
}
