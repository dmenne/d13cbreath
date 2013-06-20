#' @title Exponential beta function to fit 13C breath test PDR
#' 
#' @description Functions to fit PDR/DOB data to exponential-beta formula given in 
#' Sanaka M, Nakada K (2010) Stable isotope breath test for assessing gastric emptying:
#' A comprehensive review.  J. Smooth Muscle Research 46(6): 267-280
#' 
#' Bluck L J C and Coward W A 2006 Measurement of gastric
#' emptying by the C-13-octanoate breath test --- rationalization
#' with scintigraphy Physiol. Meas. 27 279?89
#' 
#' For a review, see 
#'
#' Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate 
#' breath test for gastric emptying. Journal of Breath Research, 3 1-8 
#' 
#' This is the same equation as (4)  in 
#' 
#' The Wagner-Nelson Method Can Generate an Accurate Gastric Emptying Flow Curve from 
#' 13CO2 Data Obtained by a 13C-Labeled Substrate Breath Test
#' Masaki Sanaka, Takatsugu Yamamoto, Tarou Ishii, Yasushi Kuyama
#'
#' @name ExpBeta
# ExpBeta= expression(m*D*k*beta*(1-exp(-k*time))^(beta-1)*exp(-k*time))
# deriv(ExpBeta,c("m","k","beta"))
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' 
#' @param time vector of time values in minutes
#' @param Dose in mg
#' @param m efficiency
#' @param k time constant
#' @param beta form factor
#' @return values and gradients of estimated PDR for use with nls and nlme
#' @examples
#' start = list(m=20,k=1/100,beta=2)
#'
#' # Fit to real data set and show different t50 results
#' sampleFile = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")  
#' # Time 0 must be removed to avoid singularity
#' breathID = ReadBreathId(sampleFile)
#' data = subset(breathID$Data,Time >0)
#' sample.nls = nls(PDR~ExpBeta(Time,100,m,k,beta),data=data,start=start)
#' data$PDRFitBluck=predict(sample.nls)
#' plot(data$Time,data$PDR,pch=16,cex=0.7,xlab="time (min)",ylab="PDR",
#'   main="t50 with different methods")
#' lines(data$Time,data$PDRFitBluck,col="blue")
#' t50 = t50BluckCoward(coef(sample.nls))
#' t50Maes = t50Maes(coef(sample.nls))
#' t50Scint = t50MaesScintigraphy(coef(sample.nls))
#' abline(v=t50,col="red")
#' abline(v=t50Maes,col="darkgreen",lty=2)
#' abline(v=breathID$T50,col="black",lty=4)
#' abline(v=t50Scint,col="gray",lty=3)
#' text(t50,0,"Self-corrected Bluck Coward",col="red",adj=-0.01)
#' text(breathID$T50,0.5,"From BreathID device",col="black",adj=-0.01)
#' text(t50Scint,1,"Maes scintigraphic",col="gray",adj=-0.01)
#' text(t50Maes,1.5,"Classic Maes",col="darkgreen",adj=-0.01)
#'
#' # Simulated Data set
#' Dose = 100
#' set.seed(4711)
#' # Do not use time 0, this gives singular gradients
#' # If required, shift time=0 by a small positive amount, e.g 0.1
#' # Create simulated data
#' pdr  = data.frame(time=seq(2,200,by=10))
#' pdr$PDR = 
#'   ExpBeta(pdr$time,100,start$m,start$k,start$beta)+rnorm(nrow(pdr),0,1)
#' par(mfrow=c(2,1))
#' # Plot raw data
#' plot(pdr$time,pdr$PDR,pch=16,cex=0.5,xlab="time (min)",ylab="PDR")
#' # Compute fit
#' pdr.nls = nls(PDR~ExpBeta(time,100,m,k,beta),data=pdr,start=start)
#' # Compute prediction
#' pdr$PDRfit= predict(pdr.nls)
#' lines(pdr$time,pdr$PDRfit,col="red",lwd=2)
#' 
#' # Plot cumulative
#' plot(pdr$time,CumExpBeta(pdr$time,100,coef(pdr.nls)),type="l",
#'      xlab="time (min)", ylab="cPDR")
#' # Show t50
#' t50 = t50BluckCoward(coef(pdr.nls))
#' tlag = tLagBluckCoward(coef(pdr.nls))
#' abline(v=t50,col="gray")
#' abline(v=tlag,col="green")
#' abline(h=50,col="gray")
#'
#'
#' # Create simulated data from several patients
#' pdr1 = data.frame(patient=as.factor(LETTERS[1:10]))
#' pdr1$m = start$m*(1+rnorm(nrow(pdr1),0,0.1))
#' pdr1$k = start$k*(1+rnorm(nrow(pdr1),0,0.3))
#' pdr1$beta = start$beta*(1+rnorm(nrow(pdr1),0,0.1))
#' pdr1  = merge(pdr1,expand.grid(time=seq(2,200,by=10),patient=LETTERS[1:10]))
#' pdr1 = pdr1[order(pdr1$patient,pdr1$time),]
#'
#' # Simulated case: for patient A, only data up to 50 minutes are available
#' pdr1 = pdr1[!(pdr1$patient=="A" & pdr1$time > 50),]
#' set.seed(4711)
#' pdr1$PDR =
#'   with(pdr1, ExpBeta(time,100,m,k,beta)+rnorm(nrow(pdr1),0,1))
#'
#' # Compute nls fit for patient A only: fails
#' # The following line will produce an error message
#' pdr.nls = try(nls(PDR~ExpBeta(time,100,m,k,beta),data=pdr1,start=start,
#'                   subset=patient=="A"))
#' stopifnot(class(pdr.nls)=="try-error")
#'
#' # Use nlme to fit the whole set with one truncated record
#' library(nlme)
#' library(lattice)
#' library(latticeExtra)
#' pdr.nlme = nlme(PDR~ExpBeta(time,100,m,k,beta),data=pdr1,
#'                 fixed= m+k+beta~1,
#'                 random = m+k+beta~1,
#'                 groups=~patient,
#'                 start=c(m=20,k=1/100,beta=2))
#' coef(pdr.nlme)    
#' predData = expand.grid(time=seq(0,400,10),patient=LETTERS[1:10])
#' predData$PDR = predict(pdr.nlme,newdata=predData)
#' predData$what = "fit"
#' pdr1$what = "data"
#' predData = rbind(pdr1[,c("patient","time","PDR","what")],predData)
#' predData$what = as.factor(predData$what)  
#' xyplot(PDR~time|patient,groups=what,data=predData,cex=0.4,pch=16, type=c("p","l"),
#'     distribute.type=TRUE, 
#'     main="Patient A gives a good fit with few data using nlme.
#'     Borrowing strength in action!")
#' @export

ExpBeta= function(time,Dose,m,k,beta){
  .expr1 <- m * Dose
  .expr2 <- .expr1 * k
  .expr3 <- .expr2 * beta
  .expr6 <- exp(-k * time)
  .expr7 <- 1 - .expr6
  .expr8 <- beta - 1
  .expr9 <- .expr7^.expr8
  .expr10 <- .expr3 * .expr9
  .expr20 <- .expr6 * time
  .value <- .expr10 * .expr6
  .grad <- array(0, c(length(.value), 3L), list(NULL, c("m", 
                                                        "k", "beta")))
  .grad[, "m"] <- Dose * k * beta * .expr9 * .expr6
  .grad[, "k"] <- (.expr1 * beta * .expr9 + .expr3 * (.expr7^(.expr8 - 
                                                                1) * (.expr8 * .expr20))) * .expr6 - .expr10 * .expr20
  .grad[, "beta"] <- (.expr2 * .expr9 + .expr3 * (.expr9 * 
                                                    log(.expr7))) * .expr6
  attr(.value, "gradient") <- .grad
  .value
}

#' @title Cumulative Exponential Beta function
#' @description Equation (2), page 4 from Bluck, "Recent advances in the interpretation of
#' the 13C octanoate breath test for gastric emptying". This is the cumulative beta exponential, 
#' and can be used to compute the 50% points.
#'
#' @name CumExpBeta
#' @param Time in minutes
#' @param Dose in mg
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required.
#' Note that \code{k} is measured in 1/min (e.g. 0.01/min), 
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return vector of predicted cumulative PDR
#' @seealso \code{\link{ExpBeta}}
#' @export
CumExpBeta  = function(Time,Dose,cf){
  if  (!is.numeric(cf)) 
    stop("CumExpBeta requires a vector, does not work for data frames")
  ekt = 1-exp(-cf["k"]*Time)
  beta = cf["beta"]
  unlist(Dose*(beta*(ekt)^(beta-1)-(beta-1)*ekt^beta))
}
