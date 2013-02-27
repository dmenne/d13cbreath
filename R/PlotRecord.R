#' @title Plot a 13C record and fit results
#'  
#' @description Reads record from database and plots raw data,
#' fit, and extracted coefficients
#' 
#' @param con Open connection to SQLite database; use \code{OpenSqliteConnection}
#' to connect.
#' 
#' @param breathTestRecordID BreathTestRecordID in database; used in tables 
#' BreathTestRecord (primary), BreathTestParameter, BreathTestTimeSeries (foreign).
#' @param showParameters The parameters to display in the graph as a data frame with
#' column \code{Parameter} (t50, tlag,GEC) and \code{Method} (BreathID, BluckCoward, Ghoos).
#' If Method is NA, all variants of Parameter are shown. By default, shows all
#' variants of t50.
#' @return A ggplot2 graphics 
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' if (exists("con")) suppressWarnings(dbDisconnect(con))
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateEmptyBreathTestDatabase(sqlitePath)
#' filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
#' con = OpenSqliteConnection(sqlitePath)
#' breathTestRecordID = AddBreathTestRecord(filename,con)
#' showParameters = data.frame(Parameter="t50",Method = c("BreathID","BluckCoward"))
#' Plot13CRecord(con,breathTestRecordID)
#' Plot13CRecord(con,breathTestRecordID, showParameters)
#' dbDisconnect(con)
#' @import ggplot2
#' @import RSQLite
#' @import RColorBrewer
#' @export
Plot13CRecord = function(con, breathTestRecordID, showParameters=NULL) {
  q = str_c("SELECT Time, Value as PDR from BreathTestTimeSeries where 
Time > 0 and Parameter = 'PDR' and BreathTestRecordID = ",
            breathTestRecordID)
  ts = dbGetQuery(con,q)
  if (nrow(ts)==0)
    stop(str_c("No time series found for BreathTestRecordID ",breathTestRecordID))
  ts = cbind(what="Data",ts)
  q = str_c(
    "SELECT Parameter, Method, Value from BreathTestParameter where BreathTestRecordID = ",
    breathTestRecordID," ORDER BY Parameter, Method")
  parm = dbGetQuery(con,q)
  if (nrow(parm)==0)
    stop(str_c("No parameters found for BreathTestRecordID ",breathTestRecordID))

  q = str_c("SELECT * from BreathTestRecord where BreathTestRecordID = ",
    breathTestRecordID)
  rec = dbGetQuery(con,q)
  # Find out what parameters to show
  if (is.null(showParameters)) {
    showParameters = data.frame(Parameter="t50",Method=NA)
  }
  # Debug
  #showParameters = data.frame(Parameter="t50",Method = c("BreathID","BluckCoward"))
  
  showParameters1 = showParameters[is.na(showParameters$Method),"Parameter",drop=FALSE]
  showParameters2 = showParameters[!is.na(showParameters$Method),]
  showPars = rbind(merge(parm,showParameters1),merge(parm,showParameters2))
  showPars$text = str_c(showPars$Parameter," ",showPars$Method)
  # Compute prediction
  stepMinutes = 2 # 
  pred = data.frame(what = "Pred", 
        Time = c(min(ts$Time),seq(stepMinutes,max(ts$Time*1.2),by=stepMinutes)))
  bcPars = parm[parm$Parameter %in% c("beta","k","m"),c("Parameter","Value")]
  rownames(bcPars)=bcPars$Parameter
  pred$PDR = as.vector(BluckCoward(pred$Time,rec$Dose,bcPars["m","Value"],
                         bcPars["k","Value"],bcPars["beta","Value"]) )

  ylim = c(min(ts$PDR), max(ts$PDR)*1.02)
  showPars$itext = as.integer(as.factor(showPars$text))*ylim[2]*0.03
  showPars$yend = ylim[2]

  Time=PDR=Value=itext=yend=NULL # Avoid "no visible binding"
  
  g = ggplot(data=ts,aes(x=Time,y=PDR))+geom_point()+
    ylim(ylim) +
    geom_line(data=pred,aes(x=Time,y=PDR),col=1,lwd=1.5) +
    geom_segment(aes(x=Value, y=itext, xend=Value, yend=yend ,col=text),
                   linetype=4, lwd=1, data=showPars, show_guide=FALSE) +
    geom_text(aes(label=text, x=Value, y=itext, col=text),
                adj=-0.04,data=showPars,show_guide=FALSE)
  g + theme_bw() +
      scale_colour_manual( values=brewer.pal(6,"Dark2")) +
      theme(panel.grid.major=element_blank())
}
  

