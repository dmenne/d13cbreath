#' @title Breathtest population fit
#' @description
#' Fits a population \code{nlme} model to multiple breath test PDR records.
#' First, separate nls fits are done to determine those records that do not give
#' a valid fit. Technically it would be possible to fit these outliers with
#' a population fit, but this leads to failure of the method quite often, so
#' the simple outlier removal method was chosen.
#' #' @name BreathTestPopulationFit
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @param x data frame with columns \code{BreathTestRecordID, Time, PDR}.
#' When no parameter is given, reads from default database via with function
#' \code{GetPopulationsData}.
#' @return data frame with columns \code{BreathTestRecordID, m, k,beta}
#' of the population fit. Use function \code{SavePopulationFit} to save to database.
#' @examples
#' set.seed(14024)
#' sqliteFile = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(sqliteFile)
#' pd = GetPopulationData(con)
#' # Make one stupid outlier
#' pp = pd$PDR[pd$BreathTestRecordID=="1"]
#' pp = pp+(1:length(pp))*0.5
#' pd$PDR[pd$BreathTestRecordID==1] = pp
#' head(pd)
#' # This gives a "did not converge" error
#' cf = try(BreathTestPopulationFit(pd))
#' print(cf)
#' dbDisconnect(con)
#' @import nlme
#' @export
#'
BreathTestPopulationFit = function(x = NULL) {
  # Get data from default directory if not given
  if (is.null(x))
    x = GetPopulationData()
  #x=pd # Testing
  if (nrow(x) == 0)
    return(NULL)
  start = c(m = 30,k = 0.01,beta = 1.8)
  bc.nls <- suppressWarnings(nlsList(
    PDR ~ ExpBeta(Time,100,m,k,beta) | BreathTestRecordID,
    data = x,start = start
  ))
  bc.nlme = suppressWarnings(try(nlme(
    PDR ~ ExpBeta(Time,100,m,k,beta),
    data = x,
    control = nlmeControl(pnlsTol = 3),
    fixed = m + k + beta ~ 1,
    random = m + pdDiag(k + beta) ~ 1,
    groups = ~ BreathTestRecordID,
    start = fixef(bc.nls)
  ),silent = TRUE)
  )
  success = !inherits(bc.nlme,"try-error")
  if (!success)
    # This should work in most cases, since we removed all nlsList failures
    stop("Population fit did not converge")
  cf = coef(bc.nlme)
  cf = cbind(BreathTestRecordID = rownames(cf),cf)
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
SavePopulationFit = function(cf,con = NULL) {
  localCon = is.null(con) # Open default database on NULL
  if (localCon)
    con = OpenSqliteConnection()
  
  Method = c(
    "ExpBetaPop","ExpBetaPop","ExpBetaPop",
    "BluckCowardPop","MaesPop","MaesScintPop",
    "BluckCowardPop","MaesPop"
  )
  # Delete all old entries
  q = paste0(
    "DELETE FROM BreathTestParameter where Method in ('",
    paste0(unique(Method),collapse = "','"),"')"
  )
  dbExecute(con,q)
  for (i in 1:nrow(cf))
  {
    cf1 = cf[i,]
    pars = data.frame(
      BreathTestRecordID = cf1$BreathTestRecordID,
      Parameter = c("m","k","beta","t50","t50","t50","tlag","tlag"),
      Method = Method,
      Value = unlist(
        c(
          cf1["m"],cf1["k"],cf1["beta"],
          t50BluckCoward(cf1),
          t50Maes(cf1),
          t50MaesScintigraphy(cf1),
          tLagBluckCoward(cf1),tLagMaes(cf1)
        )
      )
    )
    ret = try(
      SaveBreathTestParameters(con, pars),
      silent = TRUE
    )
    if (inherits(ret,"try-error"))
      stop(paste0(
        "Could not write fit parameters for RecordID",cf1$BreathTestRecordID)
      )
  }
  if (localCon)
    dbDataType(con)
  # Return
  ret = data.frame(
    BreathTestRecordID = c(as.integer(levels(
      cf$BreathTestRecordID
    )),
    as.integer(attr(cf,"removed"))),
    status = c(rep("kept",nrow(cf),),
               rep("removed", length(
                 attr(cf,"removed")
               )))
  )
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
GetPopulationData = function(con = NULL) {
  localCon = is.null(con)
  if (localCon)
    con = OpenSqliteConnection()
  q = "SELECT BreathTestRecordID,Time,Value as PDR from BreathTestTimeSeries where Parameter='PDR' and Time >0"
  # Use these as initial values
  q1 = "SELECT BreathTestRecordID, Parameter, Value from BreathTestParameter where Method='BluckCoward' order by BreathTestRecordID,Value "
  pars = dbGetQuery(con,q1)
  x = dbGetQuery(con,q)
  if (localCon)
    dbDisconnect(con)
  x$BreathTestRecordID = as.factor(x$BreathTestRecordID)
  if (nrow(x) == 0)
    stop("GetPopulationData: No data found")
  x
}


#' @title Recompute population fit and store results in database
#' @description
#' The population fit using nlme is computed, and coefficients are saved in database,
#' overwriting old ones.
#' @name RebuildPopulationFitDatabase
#' @param con connection to SQlite database. If no connection given, opens connection
#' to default database with path given by \code{getOption("Gastrobase2SqlitePath")}
#' @export
RebuildPopulationFitDatabase = function(con = NULL) {
  localCon = is.null(con)
  if (localCon)
    con = OpenSqliteConnection()
  x = GetPopulationData(con)
  cf = BreathTestPopulationFit(x)
  res = SavePopulationFit(cf,con) # Return a data frame with kept/removed
  if (localCon)
    dbDisconnect(con)
  kept = sum(res$status == "kept")
  removed = sum(res$status == "removed")
  paste0("Population fit:",
         kept, " Records added\n",
         removed," Records not added\n")
}
