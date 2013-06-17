suppressPackageStartupMessages(library(D13CBreath))
library(ggplot2)
library(lattice) # for splom
library(reshape2)
library(stringr)
library(RColorBrewer)
suppressPackageStartupMessages(library(hdrcde))
options(shiny.usecairo = FALSE)
#options(error=browser)
maxOutlier = 3

theme_set(theme_bw()+theme(panel.margin=grid::unit(0,"lines")))
#selectedTab = "Splom"
methodParameters =  c("BluckCoward t50/tlag", "BluckCowardPop t50/tlag",
    "Maes t50/tlag","MaesPop t50/tlag","ExpBeta k/m","ExpBeta k/beta",
                      "WN t50/BluckCoward t50")
helpMethodConditions = list()
helpMethod = list()
helpTexts = list()
helpTexts[methodParameters[[1]]] = 
"Self-correcting method after Bluck/Coward. Starting from the Maes/Ghoos fit
of an exponential beta, it uses the terminal falling slope, i.e. parameter k, to 
correct the original Maes/Ghoos estimate for t50 for the effect of the bicarbonate pool. 
This value of t50 is always smaller than that of Maes/Ghoos; the difference is most 
pronounced for curves with a slow drop of the terminal slope. 
The lag time tlag can be negative."

helpTexts[methodParameters[[2]]] = 
"The population variant of Bluck/Coward. It could be
more stable for extreme cases, but values mostly are very close to the single-fit variant."
                      
helpTexts[methodParameters[[3]]] = 
 "Set of coefficients from the exponential beta
 fit. The term 'Maes (,Ghoos et al.)' is used here to distinguish it from the gamma function
 used in the original paper by Ghoos. The values are generally much high compared to those from
 scintigraphic/MRI data. A scintigraphic linear correction is sometimes applied,
 which is not very well justified."

        
helpTexts[methodParameters[[4]]] = 
 "The population variant of Maes/Ghoos. It could be
  more stable for extreme cases, but values mostly are very close to the single-fit variant."

helpTexts[methodParameters[[5]]] =  "Two parameters k and m from the exponential beta 
 curve fit, the third not shown here being beta.
 Other coefficients for all but Wagner-Nelson are derived from these values. Parameter k
 is the inverse time constant for the terminal slope; it is measured in 1/min, to
 compare it with values in the literatur given in 1/h, multiply by 60. A value of 
 k=0.011/m or k=0.65/h is often used in the literature as default when the records are
 too short, but this assumption is highly dubious. m is  the percentage of 
 breath-metabolized substrate, and should be < 100."

helpTexts[methodParameters[[6]]] = "Two parameters k and beta from the exponential beta
fit, the third not shown here being m.  Other coefficients for all but Wagner-Nelson are derived from these values. Parameter k
 is the inverse time constant for the terminal slope; it is measured in 1/min, to
 compare it with values in the literatur given in 1/h, multiply by 60. A value of 
 k=0.011/m or k=0.65/h is often used in the literature as default when the records are
 too short, but this assumption is highly dubious. beta is related to the initial delay,
 it typically has values between 1.5 and 3."
          
helpTexts[methodParameters[[7]]] =  "The WN (Wagner-Nelson) method is a semiparametric fit
that does not directly use the data from the exponential beta fit. It should be more 
reliable if the curve is odd-shaped, e.g. when it has double peaks. However, uses a 
correction for the bicarbonate pool that is similar to the self-corrected Bluck/Coward 
method and requires an estimate of the terminal slope. This estimate comes from the 
fit to the curve, and therefore WN and Bluck-Coward give similar results. Only when no 
curve can be fitted, the literature default of 0.65/h is used instead; this assumption 
is dubious, so it cannot be generally said that the WN method makes fewer assumptions 
about the form of the curve and the bicarbonate mechanism. The Wagner-Nelson method only
gives a value of t50 and no lag-value"

C13HelpText = list()
C13HelpText[[1]] =  "Eine 13C-Atemtest-Aufnahme kann durch die Kombination von zwei 
Parametern charakterisiert werden. Einer der Parameter ist üblicherweise der Wert für die 
Halbwertszeit (t50 oder t1/2), wobei mit der Maes/Ghoos Methode oft um den Faktor 3 größere
Werte erhalten werden als mit Wagner-Nelson (WN) und BluckCoward. Der andere ist 
ein Parameter, der als 'Lag' interpretiert wird; er ist ein Maß dafür, ob die Kurve
sind einschleicht wie eine Parabel, oder ansteigt wie eine Gerade." 

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
