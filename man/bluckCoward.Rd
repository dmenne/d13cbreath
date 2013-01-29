\name{bluckCoward}
\alias{bluckCoward}
\title{Fit Bluck/Coward function to 13C breath test data}
\usage{
  bluckCoward(time, Dose, m, k, beta)
}
\arguments{
  \item{time}{vector of time in minutes}

  \item{Dose}{in mg}

  \item{m}{efficiency}

  \item{k}{time constant}

  \item{beta}{form factor}
}
\value{
  list with fields \code{FileName, StartTime, EndTime,
  PatientNumber, PatientID, TestNo, Type, Dose, Height,
  Weight, T50, TLag, GEC}, and \code{Data} as a data frame
  of \code{Time, DOB, PDR, PDRfit, CPDR, CPDRfit}
}
\description{
  Functions to fit PDR/DOB data to formula given in Bluck L
  J C and Coward W A 2006 Measurement of gastric emptying
  by the C-13-octanoate breath test --- rationalization
  with scintigraphy Physiol. Meas. 27 279?89 For a review,
  see Bluck LJC (2009) Recent advances in the
  interpretation of the 13C octanoate breath test for
  gastric emptying. Journal of Breath Research, 3 1-8
}
\examples{
Dose = 100
start = list(m=20,k=1/100,beta=2)
set.seed(4711)
# Do not use time 0, this gives singular gradients
# If required, shift time=0 by a small positive amount, e.g 0.1
# Create simulated data
pdr  = data.frame(time=seq(2,200,by=10))
pdr$PDR =
  bluckCoward(pdr$time,100,start$m,start$k,start$beta)+rnorm(nrow(pdr),0,1)
par(mfrow=c(2,1))
# Plot raw data
plot(pdr$time,pdr$PDR,pch=16,cex=0.5,xlab="time (min)",ylab="PDR")
# Compute fit
pdr.nls = nls(PDR~bluckCoward(time,100,m,k,beta),data=pdr,start=start)
# Compute prediction
pdr$PDRfit= predict(pdr.nls)
lines(pdr$time,pdr$PDRfit,col="red",lwd=2)

# Plot cumulative
plot(pdr$time,bluckCoward2(pdr$time,100,coef(pdr.nls)),type="l",
     xlab="time (min)", ylab="cPDR")
# Show t50
t50 = t50BluckCoward2(coef(pdr.nls))
abline(v=t50,col="red")
abline(h=50,col="red")


# Create simulated data from several patients
pdr1 = data.frame(patient=as.factor(LETTERS[1:10]))
pdr1$m = start$m*(1+rnorm(nrow(pdr1),0,0.1))
pdr1$k = start$k*(1+rnorm(nrow(pdr1),0,0.3))
pdr1$beta = start$beta*(1+rnorm(nrow(pdr1),0,0.1))
pdr1  = merge(pdr1,expand.grid(time=seq(2,200,by=10),patient=LETTERS[1:10]))
pdr1 = pdr1[order(pdr1$patient,pdr1$time),]

# Simulated case: for patient A, only data up to 50 minutes are available
pdr1 = pdr1[!(pdr1$patient=="A" & pdr1$time > 50),]
set.seed(4711)
pdr1$PDR =
  with(pdr1, bluckCoward(time,100,m,k,beta)+rnorm(nrow(pdr1),0,1))

pdr.nls = try(nls(PDR~bluckCoward(time,100,m,k,beta),data=pdr1,start=start,
                  subset=patient=="A"))
stopifnot(class(pdr.nls)=="try-error")

library(nlme)
library(lattice)
library(latticeExtra)
pdr.nlme = nlme(PDR~bluckCoward(time,100,m,k,beta),data=pdr1,
                fixed= m+k+beta~1,
                random = m+k+beta~1,
                groups=~patient,
                start=c(m=20,k=1/100,beta=2))
coef(pdr.nlme)
predData = expand.grid(time=seq(0,400,10),patient=LETTERS[1:10])
predData$PDR = predict(pdr.nlme,newdata=predData)
predData$what = "fit"
pdr1$what = "data"
predData = rbind(pdr1[,c("patient","time","PDR","what")],predData)
predData$what = as.factor(predData$what)
xyplot(PDR~time|patient,groups=what,data=predData,cex=0.4,pch=16, type=c("p","l"),
       main="Patient A has a good fit with nlme with few data")
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

