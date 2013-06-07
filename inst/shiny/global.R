library(plyr)
library(D13CBreath)
library(ggplot2)
library(lattice) # for splom
library(reshape2)
library(stringr)

theme_set(theme_bw()+theme(panel.margin=grid::unit(0,"lines")))
selectedTab = "Splom"

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
  s$Record = as.integer(str_extract(s$Value,"(\\d+)"))
  s$Color = tolower(str_sub(s$SettingID,1,nchar(s$SettingID)-4))
  ddply(s,.(SettingID),function(x){
    if (x$isPatient){
      q = paste(
        "SELECT BreathTestRecordID from BreathTestRecord where PatientID=",
        x$Record) 
      p = dbGetQuery(con,q)[,1]
      if (length(p)>0)
        data.frame(color=x$Color,PatientID = x$Record, 
                   BreathTestRecordID=p) else NULL
    }
    else{
      q = paste("SELECT PatientID from BreathTestRecord where BreathTestRecordID=",
                x$Record)
      PatientID = as.integer(dbGetQuery(con,q)[1,1])
      data.frame(color=x$Color,PatientID = PatientID,
                 BreathTestRecordID=x$Record)
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
    scale_color_manual(values = levels(d$color))+  
    geom_line(data=dPred,aes(x=Time,y=ExpBeta,
                             col=BreathTestRecordID))
}


PlotPairs = function(parc,quantiles){
  parp = parComb[parComb$Pair %in% parc,1:2]
  if (nrow(parp)<2) 
    return (NULL)
  marked = MarkedRecords()
  marked$color = as.character(marked$color)
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
  p = join(p,marked[,c("color","BreathTestRecordID")],by="BreathTestRecordID")
  p$color[is.na(p$color)] = "gray"
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
#parc = parComb[9:14,"Pair"]
#quantiles = 1
#PlotPairs(parc,quantiles)
