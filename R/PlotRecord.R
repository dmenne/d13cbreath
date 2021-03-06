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
#' column \code{Parameter} (t50, tlag,GEC) and \code{Method} (BreathID, BluckCoward, Maes).
#' If Method is NA, all variants of Parameter are shown. By default, shows all
#' variants of t50.
#' @param ymax Vertical scaling; default of NULL is for autoscaling
#' @param xmax Time axis scaling; default of NULL is for autoscaling
#' @param showName Show full patient name and DOB, initials otherwise
#' @param showPopulationFit If available, show population fit as a red curve. If the
#' single fit fails, and there is a valid population fit, this is always shown.
#' @return A ggplot2 graphics object
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' if (exists("con")) suppressWarnings(dbDisconnect(con))
#' sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
#' unlink(sqlitePath)
#' CreateEmptyBreathTestDatabase(sqlitePath)
#' con = OpenSqliteConnection(sqlitePath)
#' filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
#' breathTestRecordID = AddBreathTestRecord(filename,con)
#' showParameters = data.frame(Parameter="t50",Method = c("BreathID","BluckCoward"))
#' Plot13CRecord(con,breathTestRecordID)
#' Plot13CRecord(con,breathTestRecordID, showParameters)
#' dbDisconnect(con)
#' @import ggplot2
#' @import RSQLite
#' @import RColorBrewer
#' @export
Plot13CRecord = function(con, breathTestRecordID, showParameters = NULL,
                         ymax = NULL, xmax = NULL, showName = FALSE,
                         showPopulationFit = FALSE) {
  # Debug
  #showParameters=data.frame(Parameter="t50",Method="BluckCoward")
  #breathTestRecordID = 25; showName = FALSE; ymax=NULL; ymax=0;showPopulationFit=FALSE
  #
  # Compute prediction
  stepMinutes = 2 #
  ## Local Function
  GetPrediction = function(method) {
    bcPars = parm[parm$Parameter %in% c("beta","k","m") &
                    parm$Method == method,
                  c("Parameter","Value")]
    pred = NULL
    if (nrow(bcPars) == 3) {
      pred = data.frame(what = paste0("Pred",str_sub(method,12,14)),
                        Time = c(rangeTs[1],seq(stepMinutes,rangeTs[2],by =
                                                  stepMinutes)))
      rownames(bcPars) = bcPars$Parameter
      Dose = rec$Dose
      pred[,method] = as.vector(ExpBeta(pred$Time,Dose,bcPars["m","Value"],
                                        bcPars["k","Value"],bcPars["beta","Value"]))
    }
    pred
  }
  ## End Local function
  
  q = paste0(
    "SELECT Time, Value as PDR from BreathTestTimeSeries where
    Time > 0 and Parameter = 'PDR' and BreathTestRecordID = ",
    breathTestRecordID
  )
  ts = dbGetQuery(con,q)
  if (nrow(ts) == 0)
    stop(paste0(
      "No time series found for BreathTestRecordID ",breathTestRecordID
    ))
  ts = cbind(what = "Data",ts)
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
  # Find out what parameters to show
  if (is.null(showParameters)) {
    showParameters = data.frame(Parameter = "t50",Method = NA)
  }
  # Debug
  #showParameters = data.frame(Parameter="t50",Method = c("BreathID","BluckCoward"))
  
  showParameters1 = showParameters[is.na(showParameters$Method),"Parameter",drop =
                                     FALSE]
  showParameters2 = showParameters[!is.na(showParameters$Method),]
  showPars = rbind(merge(parm,showParameters1),merge(parm,showParameters2))
  
  rangeTs = c(min(ts$Time),max(ts$Time * 1.2))
  pred = GetPrediction("ExpBeta")
  # We show population fit even if not requested when the single fit fails
  if (is.null(pred) || showPopulationFit)
    predPop = GetPrediction("ExpBetaPop")
  else
    predPop = NULL
  
  ylim = c(min(c(ts$PDR,0)), max(ts$PDR) * 1.02) # Autoscaling
  if (!is.null(ymax))
    # Manual scaling overrides
    ylim = c(0,max(ylim[2],ymax))
  
  xlim = c(0, ifelse(is.null(pred), 240,max(pred$Time))) # Autoscaling
  if (!is.null(xmax))
    # Manual scaling overrides
    xlim[2] = xmax
  
  
  Time = PDR = Value = itext = yend = NULL # Avoid "no visible binding"
  title =   format(strptime(rec$StartTime, "%Y-%m-%d"),"%d.%m.%Y")
  if (showName) {
    title = paste0(rec$FirstName," ",rec$Name, ", Test  ",title)
  } else {
    title = paste0(rec$PatientID, " (",rec$Initials, "),  Test  ",title)
  }
  g = ggplot(data = ts,aes(x = Time,y = PDR)) +
    geom_point() +
    ggtitle(title)  +
    xlab("Zeit (min)") +
    ylab("% 13C-dose recovered (PDR)") +
    scale_x_continuous(breaks = seq(0,xlim[2] + 60,by = 60)) +
    coord_cartesian(ylim = ylim,xlim = xlim)
  
  # Position pars
  if (nrow(showPars) > 0) {
    showPars = showPars[order(showPars$Value),]
    showPars$text = paste0(showPars$Parameter," ",showPars$Method,": ",
                          round(showPars$Value), " min")
    showPars$text = str_replace(showPars$text,"WN","Wagner-Nelson")
    showPars$itext = (1:nrow(showPars)) * ylim[2] * 0.03
    showPars$yend = ylim[2]
    g = g +
      geom_segment(
        aes(
          x = Value, y = itext, xend = Value, yend = yend
        ),
        col = "gray",linetype = 1, lwd = 0.4, data = showPars, show.legend =
          FALSE
      ) +
      geom_text(
        aes(label = text, x = Value, y = itext), col = "darkgreen",cex = 5,
        adj = -0.04,data = showPars, show.legend = FALSE
      )
    
  }
  ExpBeta = ExpBetaPop = 0 # avoid note on build, not used
  if (!is.null(pred))
    g = g + geom_line(
      data = pred,aes(x = Time,y = ExpBeta),col = 1,lwd = 1.5
    )
  
  if (!is.null(predPop))
    g = g + geom_line(
      data = predPop,aes(x = Time,y = ExpBetaPop),col = 2,lwd = 1
    )
  g + theme_bw() +
    scale_colour_manual(values = brewer.pal(6,"Dark2")) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14, angle = 90),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      plot.title = element_text(size = 18),
      panel.grid.major = element_blank()
      #,
      #panel.background = element_rect(fill="#F7F5F1"),
      #plot.background = element_rect(fill="#E9E3D2")
    )
}
