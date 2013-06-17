#' @title DecisionPlot: 2D contour plot of breath-test variables
#' 
#' @description Creates a 2D percentile plot of a pair of breath test variables.
#' By default, the green central regions contains 50% of the data points, 
#' determined using a highest-density-interval (HDI) method.
#' 
#' @param con connection to sqlite database; if missing, uses default database.
#' @param pars all parameters in the form as read from table \code{BreathTestParameters}.
#' @param methods if one entry is given, e.g \code{ExpBeta, BluckCoward, WN, MaesPop},
#' it is combined with both \code{parameters}; if two are given, \code{method} and 
#' \code{parameters} are paired.
#' @param parameters two parameter names, e.g. \code{c("t50","tlag")}.
#' @param prob quantile probabilities; color for these is determined by Brewer palette, 
#' default \code{RdYlGn}.
#' @param main title for graph
#' @param kde.package package to use for HDI 2D kernel smoothing
#' @param brewerPalette color palette name from \code{RBrewerColor} to use
#' @param outlierFak the multiples of standard deviations to use for outlier removal. Note 
#' outliers removed from computation. It cannot be ruled out the data point you
#' are focussing on has been removed. If in doubt, try higher values, i.e. wider ranges
#' to include.
#' @param showColors e.g. \code{c("green","blue","red","orange")} to show all marked
#' items.
#' @param showPoints should data points contributing to decision be shown?
#' @param showDateLabels If TRUE, the record date of the \code{showPoints}s records are 
#' added as label.
#' @return None; as a side effect, a plot using standard graphics is generated. Note that 
#' the image may contain aliasing lines when Cairo is avaialable on your system.
#' \url{https://groups.google.com/forum/?fromgroups#!topic/shiny-discuss/ysac9FqOQLI}
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' databasePath = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(databasePath)
#' opt = par(mfrow=c(2,2))
#' DecisionPlot(con, showColors="red", main="Method ash") 
#' DecisionPlot(con, showColors=c("orange","blue"),main= "Method ks", kde.package="ks") 
#' DecisionPlot(con, method="BluckCowardPop",showColors=c("red"), main= "BluckCoward Population" ) 
#' # For Wagner-Nelson, there is no lag, we must use it from a different source
#' DecisionPlot(con, method=c("MaesPop","Maes"),parameters=c("t50","t50"),
#'  showColors=c("green","blue"),showPoints=FALSE,
#'    main="No points, Spectral", brewerPalette = "Spectral") 
#' par(opt)
#' dbDisconnect(con)
#' @import plyr
#' @import hdrcde
#' @export
#' 
DecisionPlot = function(con=NULL, 
                        pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1],                      
                        methods = "Maes", 
                        parameters = c("t50","tlag"),
                        prob = c(0.01,0.05,0.25,0.5),
                        main = "Breath test parameters",
                        kde.package=c("ash","ks"),
                        brewerPalette = "RdYlGn",
                        outlierFak = 3,
                        showColors = NULL,
                        showPoints=TRUE,
                        showDateLabels = TRUE){
  if (FALSE){ # Debug
    con = OpenSqliteConnection()
    pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1]                      
    methods=c("Maes","WN")
    parameters=c("tlag","t50")
    showColors=c("green","blue")
    showPoints=FALSE
    main="No points, Spectral"
    brewerPalette = "Spectral"  
    prob = c(0.01,0.05,0.25,0.5)
    showDateLabels = TRUE
  }
  RecordDate = color = NULL # Only to avoid notes during package build
  stopifnot(length(methods) %in% 1:2) 
  stopifnot(length(parameters)==2)
  # Check if local database is used
  isLocal = is.null(con)
  if (isLocal) 
    con = OpenSqliteConnection()
  if (length(methods)==1) methods = rep(methods,2)
  on.exit(if(isLocal) dbDisconnect(con) )
  selectPars =   (pars$Method==methods[1] & pars$Parameter == parameters[1]) |
                 (pars$Method==methods[2] & pars$Parameter == parameters[2]) 
  pp = pars[selectPars,]
  if (nrow(pp) == 0 || ncol(pp) != 4) 
    stop("Selected parameter/method combination not available")
  qPar =     dcast(pp,BreathTestRecordID~Method+Parameter,value.var="Value")
  # Remove outliers
  nOutliers = 0
  cParameters = names(qPar)[-1]
  p1 = names(qPar)[2]
  p2 = names(qPar)[3]
  if (outlierFak > 0){
    range = ldply(qPar[,cParameters], function(x) { 
      sd = outlierFak*sqrt(var(x,na.rm=TRUE))
      m = median(x,na.rm=TRUE)
      data.frame(lower=m-sd ,upper=m+sd)})
    rownames(range) = range[,".id"]
    inRange = qPar[,p1] > range[p1,"lower"] & qPar[,p1] < range[p1,"upper"]   &
              qPar[,p2] > range[p2,"lower"] & qPar[,p2] < range[p2,"upper"]
    nOutliers = nrow(qPar[!inRange,])
    qPar = qPar[inRange,  ]
  }
  qPar = na.omit(qPar)
  qPar$index = 1:nrow(qPar)
  # plot HDI graph
  sub = NULL
  if (nOutliers==1){    
    sub = paste0(nOutliers, "outlier removed")
  } else {
    sub = paste0(nOutliers, "outliers removed")    
  }
  hdr.boxplot.2d(qPar[,p1],qPar[,p2],prob, show.points=showPoints, pch=16,
                 xlab=p1, ylab=p2, 
                 kde.package=kde.package,
                 shadecols = brewer.pal(length(prob),brewerPalette ),
                 main = main,
                 pointcol="gray70",
                 sub = paste0(nOutliers, " outliers removed"),
  )
  # Display the points in color
  markedRecords = MarkedRecords(con)[,-1]
  if (!is.null(showColors)){
    markedRecords = droplevels(markedRecords[markedRecords$color %in% showColors,])
    colorRecords =  join(markedRecords,qPar[,c("BreathTestRecordID","index")], 
                         by="BreathTestRecordID")
    showPar = join(qPar[qPar$index %in% colorRecords$index,],colorRecords,
                   by="BreathTestRecordID")
    col = as.character(showPar[,"color"])
    col[col=="orange"] = "darkorange" # otherwise not visible
    col[col=="red"] = "darkred" # otherwise not visible
    points(showPar[,cParameters],col=col,cex=2,pch=16)
    # Plot arrows between those items that have a 
    colorCount = table(colorRecords$color)
    arrowColors = names(colorCount)[colorCount>1]
    
    arrowRecords = arrange(colorRecords[colorRecords$color %in% arrowColors,],RecordDate)
    arrowRecords = join(arrowRecords,qPar,by="BreathTestRecordID")
    d_ply(arrowRecords,.(color), function(arrowRecord){
      for (i in 2:(nrow(arrowRecords))){
        x0 = arrowRecord[i-1,p1]  
        y0 = arrowRecord[i-1,p2]  
        x1 = arrowRecord[i,p1]  
        y1 = arrowRecord[i,p2]  
        warnings()
        suppressWarnings(arrows(x0,y0,x1,y1,lwd=3,angle=20,code=2,col="gray95"))
        if (!is.null(warnings())){
          text(x0,y0,pos=3,"Multiple")
        }
      }
    })
    if (showDateLabels && nrow(showPar) > 0)
      text(showPar[,cParameters],labels=showPar[,"RecordDate"],cex=0.8,adj=-0.2)
  }
  invisible(NULL)
}

#' @title Reads Setting table 
#' @description Returns a data frame of the entries of the form \code{<color>Item} 
#' in the settings table. If a patient was selected, adds all records of the patient.
#' 
#' Example of Setting table in database
#' 
#' @param con Connection to sqlite database; if missing, uses default database.
#' @return Data frame with columns 
#' \code{SettingsID, color, PatientID, BreathTestRecordID, RecordDate}.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' \dontrun{
#' SettingID    Value    
#' RedItem      Patient_10574    
#' GreenItem    Record_433    
#' }
#' databasePath = CreateSimulatedBreathTestDatabase()
#' con = OpenSqliteConnection(databasePath)
#' MarkedRecords(con)
#' dbDisconnect(con)
#' @export
MarkedRecords = function(con=NULL ){
  SettingID = NULL # Keep notes quiet
  # Check if local database is used
  isLocal = is.null(con)
  if (isLocal) 
    con = OpenSqliteConnection()
  on.exit(if(isLocal) dbDisconnect(con) )
  # Get all settings that end in 'Item'
  s = dbGetQuery(con,"SELECT * from Setting where SettingID like '%Item'")
  if (nrow(s)==0) 
    return(NULL)
  s$isPatient = str_detect(s$Value,"Patient")
  s$Record = str_match(s$Value,"\\D*_(.*)")[,2]
  
  s$Color = tolower(str_sub(s$SettingID,1,nchar(s$SettingID)-4))
  ddply(s,.(SettingID),function(x){
    if (x$isPatient){
      q = paste0(
        "SELECT BreathTestRecordID,RecordDate from BreathTestRecord where PatientID='",
        x$Record,"'") 
      p = dbGetQuery(con,q)[,1:2,drop=FALSE]
      if (nrow(p)>0){
        RecordDate = as.Date(p$RecordDate)
        data.frame(color=x$Color,PatientID = as.character(x$Record), 
                   BreathTestRecordID=as.integer(p[,1]), 
                   RecordDate = RecordDate) 
      } else NULL
    }  else{
      q = paste("SELECT PatientID,RecordDate from BreathTestRecord where BreathTestRecordID=",
                x$Record)
      p = dbGetQuery(con,q)[1,,drop=FALSE]
      data.frame(color=x$Color,PatientID = as.character(p$PatientID),
                 BreathTestRecordID=as.integer(x$Record),
                 RecordDate = as.Date(p$RecordDate))
    }
  })
}
