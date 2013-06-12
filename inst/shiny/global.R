suppressPackageStartupMessages(library(D13CBreath))
library(ggplot2)
library(lattice) # for splom
library(reshape2)
library(stringr)
library(RColorBrewer)
suppressPackageStartupMessages(library(hdrcde))
options(shiny.usecairo = FALSE)

maxOutlier = 3

theme_set(theme_bw()+theme(panel.margin=grid::unit(0,"lines")))
#selectedTab = "Splom"
methodParameters =  c("BluckCoward t50/tlag", "BluckCowardPop t50/tlag",
    "Maes t50/tlag","MaesPop t50/tlag","ExpBeta k/m","ExpBeta k/beta",
                      "WN t50/BluckCoward t50")
helpMethodConditions = list()
for (s in methodParameters){
  helpMethodConditions[s] =  paste0(
   "input.tabs=='Bewertung' && input.par2D=='",s,"'")
}

if (!exists("databasePath") )
  databasePath = getOption("Gastrobase2SqlitePath")

con = OpenSqliteConnection(databasePath)
parComb = dbGetQuery(con, "SELECT DISTINCT Parameter, Method, Parameter || '/' || Method as Pair
 FROM BreathTestParameter order by Parameter, Method")

pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1]
#records = dbGetQuery(con,
#  "SELECT BreathTestRecordID, PatientID, Substrate from BreathTestRecord")


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

PlotCurves = function(showColors){
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
  recs = MarkedRecords(con)
  if (nrow(recs)==0) 
    return (NULL)
  recs = droplevels(recs[recs$color %in% showColors,])
  if (nrow(recs)==0) 
    return (NULL)
  recSQL = paste(recs$BreathTestRecordID,collapse=",")
  q = paste0("SELECT BreathTestRecordID, Time,Value from BreathTestTimeSeries ",
             "where Parameter ='PDR' and BreathTestRecordID in (",
             recSQL,")")
  d = dbGetQuery(con,q)
  if (nrow(d)== 0) 
    return (NULL)
  d = join(d,recs[,-1],by="BreathTestRecordID")
  
  q = str_c(
    "SELECT BreathTestRecordID, Parameter, Method, Value ",
    "from BreathTestParameter where BreathTestRecordID in (",
    recSQL,") ORDER BY Parameter, Method")
  parm = dbGetQuery(con,q)
  ###### Use Fixed dose ''''''''''
  dPred = NULL
  dPred = ldply(recs$BreathTestRecordID, function(BreathTestRecordID){  
    GetPrediction("ExpBeta",BreathTestRecordID,c(0,max(d$Time)),100)
  })
  if (nrow(dPred)>0)
    dPred = join(dPred,recs[,-1],by="BreathTestRecordID")
  
  d$BreathTestRecordID = as.factor(d$BreathTestRecordID)
  dPred$BreathTestRecordID = as.factor(dPred$BreathTestRecordID)
  q = qplot(x=Time,y=Value,data=d, col=BreathTestRecordID, ylab="PDR",xlab="Minuten")  +
    scale_color_manual(values =as.character(recs$color))
  if ((nrow(dPred)>0))
    q  = q + geom_line(data=dPred,aes(x=Time,y=ExpBeta, col=BreathTestRecordID))
  q
}


#png(file="c:/tmp/a.png",width=700,height=700)
#DecisionPlot(con,pars, method = "ExpBeta", parameter=c("k","beta") , outlierFak=3,
#              showColors ="green",showDateLabels=TRUE)
#DecisionPlot(con,pars,  method = "BluckCoward", parameter=c("t50","tlag"),outlierFak=3, 
#              showColors =NULL)
#dev.off()
