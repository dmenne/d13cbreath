#' @title Breathtest population fit
#' @description 
#' Fits an population \code{nlme} model to multiple records.
#'  First, separate nls fits are done to determine those records that do not give
#' a valid fit. Technically it would be possible to fit these outliers with 
#' a population fit, but this leads to failure of the method quite often, so 
#' the simple outlier removal method was chosen.
#' @name BreathTestPopulationFit
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param x data frame with columns \code{BreathTestRecordID,Time,Value}.
#' When no parameter is given, reads from default database via with function
#' \code{GetPopulationsData}.
#' @return data frame with columns \code{BreathTestRecordID, m, k,beta} 
#' of the population fit. Use function \code{SavePopulationFit} to save to database.
#' @import nlme
#' @export 
BreathTestPopulationFit = function(x=NULL){
  if (is.null(x))   x = GetPopulationData()
  if (nrow(x)== 0) return(NULL)
  start = c(m=30,k=0.01,beta=2.4)
  bc.nls = suppressWarnings(
    nlsList(PDR~BluckCoward(Time,100,m,k,beta)|BreathTestRecordID,
            data=x,start=start))
  removed = attr(attr( na.omit(coef(bc.nls)),"na.action"),"names")
  if (length(removed)>0)
    x = x[,!(x$BreathTestRecordID %in% removed)]
  bc.nlme = nlme(PDR~BluckCoward(Time,100,m,k,beta),
                 data=x,
                 fixed = m+k+beta~1,
                 random = m+k+beta~1,
                 groups= ~BreathTestRecordID,
                 start=start)
  cf = coef(bc.nlme)
  cf = cbind(BreathTestRecordID = rownames(cf),cf)
  attr(cf,"removed")= removed
  cf
}
#' @title Save population fit to breath test data
#' @description
#' Deletes all old population entries, and saves new population fits to database.
#' @name SavePopulationFit
#' @param cf data frame with columns \code{BreathTestRecordID, m, k,beta} 
#' @param con database connection. When omitted, uses default database
#' @return data frame with \code{BreathTestRecordID} and column \code{status}
#' with values \code{kept} and \code{removed}.
#' @export
SavePopulationFit = function(cf,con=NULL){
  localCon = is.null(con) # Open default database on NULL
  if (localCon)
    con = OpenSqliteConnection()

  Method = c("BluckCowardPop","BluckCowardPop","BluckCowardPop",
             "BluckCowardPop","GhoosPop","GhoosScintPop",
             "BluckCowardPop","GhoosPop")
  # Delete all old entries
  q = paste(
     "DELETE FROM BreathTestParameter where Method in ('",
     paste(unique(Method),collapse="','",sep=""),"')",sep="")
  dbSendQuery(con,q)
  for (i in 1:nrow(cf))
  {
    cf1 = cf[i,]
    pars = data.frame(BreathTestParameterID=as.integer(NA), 
                      BreathTestRecordID=cf1$BreathTestRecordID,
                      Parameter = c("m","k","beta","t50","t50","t50","tlag","tlag"),
                      Method = Method,
                      Values = unlist(c(cf1["m"],cf1["k"],cf1["beta"],
                                 t50BluckCoward2(cf1),
                                 t50Ghoos(cf1),
                                 t50GhoosScintigraphy(cf1),
                                 tLagBluckCoward(cf1),tLagGhoos(cf1)))
    )
    success = dbWriteTable(con,"BreathTestParameter",pars,append=TRUE,
                           row.names=FALSE)
    if (!success)
      stop(str_c("Could not write fit parameters for RecordID",cf1$BreathTestRecordID))
  }
  if (localCon) dbDataType(con)
  # Return
  ret = data.frame(
    BreathTestRecordID = c(as.integer(levels(cf$BreathTestRecordID)),
                            as.integer(attr(cf,"removed"))),
             status = c(rep("kept",nrow(cf),),
                        rep("removed", length(attr(cf,"removed")))))
  ret[order(ret$status,ret$BreathTestRecordID),]
}


#' @title Read Population data from database
#' @description 
#' Retrieves a dataframe for use in \code{BreathTestPopulationFit}
#' @name GetPopulationData
#' @param con connection to SQlite database. If no connection given, opens connection
#' to default database with path given by \code{getOption("Gastrobase2SqlitePath")}
#' @return data frame with columns \code{BreathTestRecordID, Time, Value} 
#' @export 
GetPopulationData = function(con=NULL){
  localCon = is.null(con)
  if (localCon)
    con = OpenSqliteConnection()
  q = "SELECT BreathTestRecordID,Time,Value as PDR from BreathTestTimeSeries where Parameter='PDR' and Time >0"
# Use these as initial values
  q1 = "SELECT BreathTestRecordID, Parameter, Value from BreathTestParameter where Method='BluckCoward' order by BreathTestRecordID,Value "
  pars = dbGetQuery(con,q1)
  x = dbGetQuery(con,q)
  if (localCon) dbDisconnect(con)
  x$BreathTestRecordID = as.factor(x$BreathTestRecordID)
  x
}
