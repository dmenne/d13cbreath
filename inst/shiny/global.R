library(plyr)
#library(stringr)
#library(lattice)
library(D13CBreath)

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
  if (nrow(parp)<2) return (NULL)
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
    prepanel.limits =  function(x,y,...) {
      quantile(p[,-1],q1,na.rm=TRUE) }
  }  else { 
    p$Pair = paste(p$Parameter,p$Method,sep="/")
    prepanel.limits = lattice:::scale.limits
  }
  p = dcast(p,BreathTestRecordID~Pair,  value.var="Value")
  splom(p[,-1],pch=16,cex=0.8,prepanel.limits=prepanel.limits,
        main = main,
        panel = function(x,y, ...){
          panel.splom(x,y,...)
          if (sameParameters)
            panel.abline(a=0,b=1,col="lightgray")
        }    )
}
