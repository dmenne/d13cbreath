library(plyr)
#library(stringr)
#library(lattice)
library(D13CBreath)

if (!exists("databasePath") )
  databasePath = getOption("Gastrobase2SqlitePath")

con = OpenSqliteConnection(databasePath)
pars = dbGetQuery(con,"SELECT * from BreathTestParameter")[,-1]
#records = dbGetQuery(con,
#  "SELECT BreathTestRecordID, PatientID, Substrate from BreathTestRecord")
dbDisconnect(con)

parComb = arrange(unique(pars[,c("Parameter","Method")]), Parameter,Method)
parComb$Pair = paste(parComb$Parameter,parComb$Method,sep="/")


PlotPairs = function(parc){
  parp = parComb[parComb$Pair %in% parc,1:2]
  if (nrow(parp)<2) return (NULL)
  sameParameters = nlevels(factor(parp$Parameter)) ==1
  p = pars[pars$Parameter %in% parp$Parameter &  pars$Method %in% parp$Method ,]
  if (nrow(p)==0) return (NULL)
  main = "Breath test parameters"
  if (sameParameters ){
    p$Pair = p$Method 
    main = paste(main, parp$Parameter[1])    
    prepanel.limits =  function(x,y,...) {
      quantile(p[,-1],c(0.01,0.99),na.rm=TRUE) }
  }  else { 
    p$Pair = paste(p$Parameter,p$Method,sep="/")
    prepanel.limits =  lattice:::scale.limits
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
