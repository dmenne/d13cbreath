library(plyr)
suppressPackageStartupMessages(library(D13CBreath))
library(ggplot2)
library(lattice) # for splom
library(reshape2)
library(stringr)
library(RColorBrewer)
suppressPackageStartupMessages(library(hdrcde))
library(Cairo)
options(warn=2)
maxOutlier = 3

theme_set(theme_bw()+theme(panel.margin=grid::unit(0,"lines")))
#selectedTab = "Splom"

if (!exists("databasePath") )
  databasePath = getOption("Gastrobase2SqlitePath")

con = OpenSqliteConnection(databasePath)
parComb = dbGetQuery(con, "SELECT DISTINCT Parameter, Method, Parameter || '/' || Method as Pair
 FROM BreathTestParameter order by Parameter, Method")

pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1]
#records = dbGetQuery(con,
#  "SELECT BreathTestRecordID, PatientID, Substrate from BreathTestRecord")

MarkedRecords = function(){
  s = dbGetQuery(con,"SELECT * from Setting where SettingID like '%Item'")
  if (nrow(s)==0) return(NULL)
  s$isPatient = str_detect(s$Value,"Patient")
  s$Record = str_match(s$Value,"\\D*_(.*)")[,2]

  s$Color = tolower(str_sub(s$SettingID,1,nchar(s$SettingID)-4))
  ddply(s,.(SettingID),function(x){
    if (x$isPatient){
      q = paste0(
        "SELECT BreathTestRecordID,RecordDate from BreathTestRecord where PatientID='",
        x$Record,"'") 
      p = dbGetQuery(con,q)[,1:2,drop=FALSE]
      if (length(p)>0)
        data.frame(color=x$Color,PatientID = as.character(x$Record), 
                   BreathTestRecordID=as.integer(p[,1]), 
                   RecordDate = as.Date(p[,2])) else NULL
    }
    else{
      q = paste("SELECT PatientID,RecordDate from BreathTestRecord where BreathTestRecordID=",
                x$Record)
      p = dbGetQuery(con,q)[1,,drop=FALSE]
      data.frame(color=x$Color,PatientID = as.character(p$PatientID),
                 BreathTestRecordID=as.integer(x$Record),
                 RecordDate = as.Date(p$RecordDate))
    }
  })
}

PlotCurves = function(){
  stepMinutes = 2
  ## Local function
  GetPrediction = function(method,BreathTestRecordID,rangeTs,Dose){
    bcPars = parm[parm$Parameter %in% c("beta","k","m") & parm$Method==method
                  & parm$BreathTestRecordID == BreathTestRecordID,
                  c("Parameter","Value")]
    pred = NULL
    if (nrow(bcPars)==3){
      pred = data.frame(
        BreathTestRecordID = BreathTestRecordID,
        Time = c(rangeTs[1],seq(stepMinutes,rangeTs[2],by=stepMinutes)))
      rownames(bcPars)=bcPars$Parameter
      pred[,method] = as.vector(ExpBeta(pred$Time,Dose,bcPars["m","Value"],
                                        bcPars["k","Value"],bcPars["beta","Value"]) )
    } 
    pred
  }
  
  # Get color-marked curves
  recs = MarkedRecords()
  recSQL = paste(recs$BreathTestRecordID,collapse=",")
  q = paste0("SELECT BreathTestRecordID, Time,Value from BreathTestTimeSeries ",
             "where Parameter ='PDR' and BreathTestRecordID in (",
             recSQL,")")
  d = dbGetQuery(con,q)
  if (nrow(d)== 0) return (NULL)
  d = join(d,recs[,-1],by="BreathTestRecordID")
  
  q = str_c(
    "SELECT BreathTestRecordID, Parameter, Method, Value ",
    "from BreathTestParameter where BreathTestRecordID in (",
    recSQL,") ORDER BY Parameter, Method")
  parm = dbGetQuery(con,q)
  ###### Use Fixed dose ''''''''''
  dPred = ldply(recs$BreathTestRecordID, function(BreathTestRecordID){
    GetPrediction("ExpBeta",BreathTestRecordID,c(0,max(d$Time)),100)
  })
  dPred = join(dPred,recs[,-1],by="BreathTestRecordID")
  
  d$BreathTestRecordID = as.factor(d$BreathTestRecordID)
  dPred$BreathTestRecordID = as.factor(dPred$BreathTestRecordID)
  qplot(x=Time,y=Value,data=d, col=BreathTestRecordID, ylab="PDR",xlab="Minuten")  +
    scale_color_manual(values =as.character(recs$color))+  
    geom_line(data=dPred,aes(x=Time,y=ExpBeta,
                             col=BreathTestRecordID))
}

PlotPairs = function(parc,quantiles){
  parp = parComb[parComb$Pair %in% parc,1:2]
  if (nrow(parp)<2) 
    return (NULL)
  sameParameters = nlevels(factor(parp$Parameter)) ==1
  q1 =  paste("(Method ='", parp$Method,
              "' and Parameter = '",parp$Parameter,"')",collapse=" or ",sep="")
  p = dbGetQuery(con,paste0(
    "SELECT BreathTestRecordID, Parameter, Method,Value from BreathTestParameter ",
    "WHERE ",q1))
  
  if (nrow(p)==0) return (NULL)
  main = "Breath test parameters"
  if (sameParameters ){
    p$Pair = p$Method 
    main = paste(main, parp$Parameter[1])    
    q1 = c(quantiles/100.,1-quantiles/100.)
    prepanel.limits = function(x,y,...) {
      quantile(pPlot,q1,na.rm=TRUE) }
  }  else { 
    p$Pair = paste(p$Parameter,p$Method,sep="/")
    prepanel.limits = lattice:::scale.limits
  }
  p = dcast(p,BreathTestRecordID~Pair,  value.var="Value")
  marked = MarkedRecords()
  if (!is.null(marked)){
    marked$color = as.character(marked$color)
    p = join(p,marked[,c("color","BreathTestRecordID")],by="BreathTestRecordID")
    p$color[is.na(p$color)] = "gray"
  } else
    p$color = "blue"
  pPlot = p[,-c(1,ncol(p))]
  splom(pPlot,pch=16,
        prepanel.limits=prepanel.limits,
        main = main,
        panel = function(x,y, ...){
          marked =  p$color!="gray"
          seq = c(which(!marked),which(marked))
          cex = ifelse(marked,1.5,0.7)[seq]
          col = p$color[seq]
          panel.splom(x[seq],y[seq],col=col,cex=cex,...)
          if (sameParameters)
            panel.abline(a=0,b=1,col="lightgray")
        })
  
}

DecisionPlot = function(con=OpenSqliteConnection(), 
                        pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1],                      
                        method = "MaesPop", 
                        parameters = c("t50","tlag"),
                        prob = c(0.01,0.05,0.25,0.5),
                        main = "Atemtest Magenleerung",
                        kde.package="ash",
                        outlierFak = 3,
                        showColors = NULL,
                        showPoints=TRUE,
                        showDateLabels = TRUE){
  stopifnot(length(parameters)==2)
  pp = pars[pars$Method==method & pars$Parameter %in% parameters,-3]
  qPar = dcast(pp,BreathTestRecordID~Parameter,value.var="Value")
  # Remove outliers
  nOutliers = 0
  if (outlierFak > 0){
    range = ldply(qPar[,parameters], function(x) { 
      sd = outlierFak*sqrt(var(x,na.rm=TRUE))
      m = median(x,na.rm=TRUE)
      data.frame(lower=m-sd ,upper=m+sd)})
    rownames(range) = range[,".id"]
    p1 = parameters[1]
    p2 = parameters[2]
    inRange = qPar[,p1] > range[p1,"lower"] & qPar[,p1] < range[p1,"upper"]   &
      qPar[,p2] > range[p2,"lower"] & qPar[,3] < range[p2,"upper"]
    nOutliers = nrow(qPar[!inRange,])
    qPar = qPar[inRange,  ]
  }
  qPar$index = 1:nrow(qPar)
  # plot HDI graph
  sub = NULL
  if (nOutliers>0)
    sub = paste0(nOutliers, " outlier", ifelse(nOutliers==1,"","s")," removed")
  hdr.boxplot.2d(qPar[,2],qPar[,3],prob, show.points=showPoints, pch=16,
                 xlab=paste(method, parameters[1]),
                 ylab=paste(method, parameters[2]), 
                 kde.package=kde.package,
                 shadecols = brewer.pal(length(prob),"RdYlGn" ),
                 main = main,
                 pointcol="gray70",
                 sub = paste0(nOutliers, " outliers removed"),
                 )
  # Display the points in color
  markedRecords = MarkedRecords()[,-1]
  if (!is.null(showColors)){
    markedRecords = droplevels(markedRecords[markedRecords$color %in% showColors,])
    colorRecords =  join(markedRecords,qPar[,c("BreathTestRecordID","index")], 
                       by="BreathTestRecordID")
    showPar = join(qPar[qPar$index %in% colorRecords$index,],colorRecords,
                 by="BreathTestRecordID")
    points(showPar[,parameters],col=as.character(showPar[,"color"]),cex=2,pch=16)
    # Plot arrows between those items that have a 
    colorCount = table(colorRecords$color)
    arrowColors = names(colorCount)[colorCount>1]
  
    arrowRecords = arrange(colorRecords[colorRecords$color %in% arrowColors,],RecordDate)
    arrowRecords = join(arrowRecords,qPar,by="BreathTestRecordID")
    
    ddply(arrowRecords,.(color), function(arrowRecord){
      for (i in 2:(nrow(arrowRecord))){
        x0 = arrowRecord[i-1,parameters[1]]  
        y0 = arrowRecord[i-1,parameters[2]]  
        x1 = arrowRecord[i,parameters[1]]  
        y1 = arrowRecord[i,parameters[2]]  
        arrows(x0,y0,x1,y1,lwd=3,angle=20,code=2,col="gray95")
      }
    })
    if (showDateLabels)
      text(showPar[,parameters],labels=showPar[,"RecordDate"],cex=0.6,adj=-0.2)
  }
  list(nOutliers=nOutliers)
}

#png(file="c:/tmp/a.png",width=700,height=700)
DecisionPlot( method = "ExpBeta", parameter=c("k","beta") , outlierFak=3,showColors ="green")
#DecisionPlot( method = "BluckCoward", parameter=c("t50","tlag"),outlierFak=3, 
#              showColors =NULL)
#dev.off()
