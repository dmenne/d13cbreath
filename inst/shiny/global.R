suppressPackageStartupMessages(library(D13CBreath))
library(ggplot2)
library(lattice) # for splom
library(reshape2)
library(stringr)
library(plyr)
library(RColorBrewer)
suppressPackageStartupMessages(library(hdrcde))
options(shiny.usecairo = FALSE)
#options(error=browser)
maxOutlier = 3

theme_set(theme_bw()+theme(panel.margin=grid::unit(0,"lines")))
#selectedTab = "Splom"
methodParameters =  c("Maes t50/tlag",
                      "BluckCoward t50/tlag", 
                      "WN t50/BluckCoward t50",
                      "ExpBeta k/m",
                      "ExpBeta k/beta")
helpMethodConditions = list()
helpMethod = list()
helpTexts = list()
helpTexts[methodParameters[[1]]] = 
 "This is the method from <i>Maes/Ghoos</i> that computes t<sub>50</sub> and t<sub>lag</sub>
  from the exponential beta curve. 
  The values for t<sub>50</sub> are much higher than those from
  scintigraphic/MRI data. A scintigraphic linear correction is sometimes applied,
  but this does not provide an advantage for classification. The lag time t<sub>lag</sub>
  is misnomed, because it is the position of PDR maximum, and only indirectly related to delay
  of the whole curve."
    
helpTexts[methodParameters[[2]]] = 
  "Self-correcting method after <i>Bluck/Coward</i>. It uses the terminal falling slope, 
 i.e. parameter k, to of the beta exponential fit to correct the original <i>Maes/Ghoos</i>
estimate for t<sub>50</sub> for the effect of the bicarbonate pool. 
This value of t<sub>50</sub> is always smaller than that of <i>Maes/Ghoos</i>; the difference 
is most pronounced for curves with a slow drop of the terminal slope. 
The lag time t<sub>lag</sub> can be negative."

helpTexts[methodParameters[[4]]] =  "This plot shows two parameters <code>k</code> and 
 <code>m</code> from the exponential beta curve fit.
 Other coefficients for all but <i>Wagner-Nelson</i> are derived from these values. Parameter <code>k</code>
 is the inverse time constant for the terminal slope; it is measured in <code>1/min</code>; to
 compare it with values in the literatur given as <code>1/h</code>, multiply by 60. A value of 
 <code>k=0.011/min</code> or <code>k=0.65/h</code> is often used in the literature as default when the records are
 too short, but this assumption is highly dubious. <code>m</code> is  the percentage of 
 breath-metabolized substrate, and should be &lt; 100."

helpTexts[methodParameters[[5]]] = "Two parameters  <code>k</code> and  <code>beta</code> 
from the exponential beta fit.  Other coefficients for all but <i>Wagner-Nelson</i> are 
derived from these values.  Parameter <code>k</code>
 is the inverse time constant for the terminal slope; it is measured in  <code>1/min</code>; to
 compare it with values in the literatur given in  <code>1/h</code>, multiply by 60. A value of 
  <code>k=0.011/min</code> or <code>k=0.65/h</code> is often used in the literature as default 
 when the records are too short to fit the terminal slope, but this assumption is highly dubious. 
 <code>beta</code> is related to the initial delay, it typically has values between 1.5 and 3."
          
helpTexts[methodParameters[[3]]] =  "The <i>WN (Wagner-Nelson)</i> method used a 
semiparametric method that does not directly use the data from the fitted curve. It 
should be more reliable if the curve is odd-shaped, e.g. when it has double peaks. However, 
it uses a correction for the bicarbonate pool that is similar to the self-corrected 
<i>Bluck/Coward</i> method and requires an estimate of the terminal slope. This estimate 
comes from the fit to the curve, and therefore <i>Wagner-Nelson</i> and <i>Bluck-Coward</i> give similar 
results. Only when no curve can be fitted, the literature default of 0.65/h is used instead; 
this assumption is dubious, so it cannot be generally said that the <i>WN</i> method makes 
fewer assumptions about the form of the curve and the bicarbonate mechanism. 
The Wagner-Nelson method only gives a value of t<sub>50</sub> and no lag-value"

C13HelpText = list()
C13HelpText[[1]] =  "Eine 13C-Atemtest-Aufnahme kann durch die Kombination von zwei 
Parametern charakterisiert werden. Einer der Parameter ist üblicherweise der Wert für die 
Halbwertszeit (t<sub>50</sub> oder t<sub>1/2</sub>), wobei mit der <i>Maes/Ghoos</i> Methode 
oft um den Faktor 3 größere
Werte erhalten werden als mit <i>Wagner-Nelson (WN)</i> und <i>Bluck-Coward</i>. Der andere ist 
ein Parameter, der als 'Lag' interpretiert wird. Theoretisch steht er für
eine Verzögerung, in Praxis ist er bei Maes/Ghoos aber die Position den Maximums 
der PDR-Kurve. Bluck nennt <i>tLag</i> deshalb <i>tMax</i>." 

C13HelpText[[2]] = "Im dunkelgrünen Bereich liegen 50% aller gemessenen Atemtestwerte
in der Datenbank; die Struktur der Bereiche wird sich ändern, wenn mehr Aufnahmen vorliegen.
Man könnte annehmen, dass die 'mittlere Mehrheit' eher normal ist; diese Annahme ist 
gefährlich, aber eine bessere gibt es mangels Gold-Standard zur Zeit nicht. Im hellgrünen 
und dunkelgrünen Bereich zusammen liegen 75%, mit orange 95%, und mit rot 99% der 
 Atemtestwerte."

C13HelpText[[3]] = 
"Wenn ein Patient mit mehreren Aufnahmen markiert wurde, zeigen weiße
Pfeile an, wie sich die Werte über die Zeit entwickelt haben."
  

for (s in methodParameters){
  helpMethodConditions[s] =  
    paste0("input.tabs=='Bewertung' && input.par2D=='",s,"'")
}

if (!exists("databasePath") )
  databasePath = getOption("Gastrobase2SqlitePath")

con = OpenSqliteConnection(databasePath)
parComb = dbGetQuery(con, "SELECT DISTINCT Parameter, Method, Parameter || '/' || Method as Pair
 FROM BreathTestParameter order by Parameter, Method")
# Temporary remove population stuf
parComb = parComb[!str_detect(parComb$Method,"Pop"),]

pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1]
#records = dbGetQuery(con,
#  "SELECT BreathTestRecordID, PatientID, Substrate from BreathTestRecord")

PlotPairs = function(parc,quantiles){
  parp = parComb[parComb$Pair %in% parc,1:2]
  if (nrow(parp)<2) 
    return (NULL)
  sameParameters = nlevels(factor(parp$Parameter)) ==1
  q1 =  paste0("(Method ='", parp$Method,
              "' and Parameter = '",parp$Parameter,"')",collapse=" or ")
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
  pPlot = na.omit(p[,-c(1,ncol(p))])
  if (nrow(pPlot)<2) return(NULL)
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
  if (is.null(recs) || nrow(recs)==0) 
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


