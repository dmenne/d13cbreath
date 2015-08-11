#' @title Compute and save Wagner-Nelson fit
#'
#' @description Reads record with given BreathTestRecordID from database and computes
#' Wagner-Nelsoen fit parameter which are written back to table BreathTestParameter
#' Existing parameters are overwritten.
#' See Sanaka, Yamamoto, Ishii, Kuyama, Digestion 2004 (69), 71-78
#'
#' @param con connection to SQlite database
#' @param BreathTestRecordID these data will be read from BreathTestTimeSeries
#' @examples
#' sqliteFile = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(sqliteFile)
#' BreathTestRecordID = 1
#' ComputeAndSaveWNFit(con,BreathTestRecordID)
#' dbDisconnect(con)
#'
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @export
#'
ComputeAndSaveWNFit = function(con,BreathTestRecordID)  {
  k = dbGetQuery(
    con, paste0(
      "SELECT Value as k from BreathTestParameter where BreathTestRecordID=",
      BreathTestRecordID," and Parameter ='k'"
    )
  )[1,1]
  if (is.na(k))
    k = 0.65 / 60 # Default value is 0.65, but we use minutes everywhere
  data = dbGetQuery(
    con,paste0(
      "SELECT Time, Value as PDR from BreathTestTimeSeries where BreathTestRecordID=" ,
      BreathTestRecordID, " and Parameter = 'PDR' and Time > 0 order by Time"
    )
  )
  if (inherits("data","try-error"))
    stop(paste("No PDR data found for BreathTestRecordID",BreathTestRecordID))
  auct = AUCt(data$Time,data$PDR)
  aucInf = auct[length(auct)] + data$PDR[nrow(data)] / k # equation 7 in Sanaka
  frac = 1 - (auct + data$PDR / k) / aucInf
  # compute 0.5-crossing
  spl = smooth.spline(data$Time,frac)
  t50 = try(uniroot(function(t)
    predict(spl,t)$y - 0.5,c(0,max(data$Time)))$root,silent = TRUE)
  if (class(t50) == "try-error")
    t50 = 0
  ret = dbSendQuery(
    con, paste0(
      "DELETE FROM BreathTestParameter where BreathTestRecordID=",
      BreathTestRecordID ," and Method ='WN'"
    )
  )
  dbClearResult(ret)
  # Write parameters and coefficients
  pars = data.frame(
    BreathTestParameterID = as.integer(NA), BreathTestRecordID,
    Parameter = "t50",Method = "WN",Value = t50
  )
  # *** Todo: remove dirty trick for invalid WN
  pars =  pars[pars$Value != 0,]
  q = str_c("INSERT INTO BreathTestParameter VALUES(",
            paste(rep("?",ncol(pars)),collapse = ","),")")
  ret = try(dbGetPreparedQuery(con, q,bind.data = pars), silent = TRUE)
  if (inherits(ret,"try-error"))
    stop(str_c("Could not write fit parameters for Record",BreathTestRecordID))
  # Compute predicted WN fit
  Time = seq(0,max(data$Time), by = 5)
  wn = data.frame(
    BreathTestTimeSeriesID = as.integer(NA),
    BreathTestRecordID = BreathTestRecordID,
    Time = Time,
    Parameter = "WN",
    Value = predict(spl,Time)$y
  )
  
  # Delete old values
  ret = dbSendQuery(
    con, paste0(
      "DELETE FROM BreathTestTimeSeries where BreathTestRecordID=",
      BreathTestRecordID ," and Parameter ='WN'"
    )
  )
  dbClearResult(ret)
  q = str_c("INSERT INTO BreathTestTimeSeries VALUES(",
            paste(rep("?",ncol(wn)),collapse = ","),")")
  ret = try(dbGetPreparedQuery(con, q,bind.data = wn), silent = TRUE)
  
  if (inherits(ret,"try-error"))
    stop(str_c(
      "Could not write predicted Wagner-Nelson data for record ",
      BreathTestRecordID
    ))
  t50
}

AUCt = function (x, y)
{
  # http://stackoverflow.com/questions/14201375/how-can-i-calculate-the-95-credible-limit-of-an-area-under-a-curve-in-r/14204138#14204138
  cumsum(c(0,diff(x) * (y[-1] + y[-length(y)])) / 2)
}
