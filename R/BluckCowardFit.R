#' @title Fit Bluck/Coward self-corrected function to 13C breath test PDR
#' 
#' @description Functions to fit PDR/DOB data to exponential-beta formula given in 
#' Sanaka M, Nakada K (2010) Stable isotope breath test for assessing gastric emptying:
#' A comprehensive review
#' J. Smooth Muscle Research 46(6): 267-280
#'
#' Bluck L J C and Coward W A 2006 Measurement of gastric
#' emptying by the C-13-octanoate breath test --- rationalization
#' with scintigraphy Physiol. Meas. 27 279?89
#' For a review, see 
#' Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate 
#' breath test for gastric emptying. Journal of Breath Research, 3 1-8 
#' @name BluckCoward
# BluckCoward = expression(m*D*k*beta*(1-exp(-k*time))^(beta-1)*exp(-k*time))
# deriv(bluck,c("m","k","beta"))
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' 
#' @param time vector of time in minutes
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
#' sample.nls = nls(PDR~BluckCoward(Time,100,m,k,beta),data=data,start=start)
#' data$PDRFitBluck=predict(sample.nls)
#' plot(data$Time,data$PDR,pch=16,cex=0.7,xlab="time (min)",ylab="PDR",
#'   main="t50 with different methods")
#' lines(data$Time,data$PDRFitBluck,col="blue")
#' t50 = t50BluckCoward2(coef(sample.nls))
#' t50Ghoos = t50Ghoos(coef(sample.nls))
#' t50Scint = t50GhoosScintigraphy(coef(sample.nls))
#' abline(v=t50,col="red")
#' abline(v=t50Ghoos,col="darkgreen",lty=2)
#' abline(v=breathID$T50,col="black",lty=4)
#' abline(v=t50Scint,col="gray",lty=3)
#' text(t50,0,"Self-corrected Bluck Coward",col="red",adj=-0.01)
#' text(breathID$T50,0.5,"From BreathID device",col="black",adj=-0.01)
#' text(t50Scint,1,"Ghoos scintigraphic",col="gray",adj=-0.01)
#' text(t50Ghoos,1.5,"Classic Ghoos",col="darkgreen",adj=-0.01)
#'
#' # Simulated Data set
#' Dose = 100
#' set.seed(4711)
#' # Do not use time 0, this gives singular gradients
#' # If required, shift time=0 by a small positive amount, e.g 0.1
#' # Create simulated data
#' pdr  = data.frame(time=seq(2,200,by=10))
#' pdr$PDR = 
#'   BluckCoward(pdr$time,100,start$m,start$k,start$beta)+rnorm(nrow(pdr),0,1)
#' par(mfrow=c(2,1))
#' # Plot raw data
#' plot(pdr$time,pdr$PDR,pch=16,cex=0.5,xlab="time (min)",ylab="PDR")
#' # Compute fit
#' pdr.nls = nls(PDR~BluckCoward(time,100,m,k,beta),data=pdr,start=start)
#' # Compute prediction
#' pdr$PDRfit= predict(pdr.nls)
#' lines(pdr$time,pdr$PDRfit,col="red",lwd=2)
#' 
#' # Plot cumulative
#' plot(pdr$time,BluckCoward2(pdr$time,100,coef(pdr.nls)),type="l",
#'      xlab="time (min)", ylab="cPDR")
#' # Show t50
#' t50 = t50BluckCoward2(coef(pdr.nls))
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
#'   with(pdr1, BluckCoward(time,100,m,k,beta)+rnorm(nrow(pdr1),0,1))
#'
#' # Compute nls fit for patient A only: fails
#' # The following line will produce an error message
#' pdr.nls = try(nls(PDR~BluckCoward(time,100,m,k,beta),data=pdr1,start=start,
#'                   subset=patient=="A"))
#' stopifnot(class(pdr.nls)=="try-error")
#'
#' # Use nlme to fit the whole set with one truncated record
#' library(nlme)
#' library(lattice)
#' library(latticeExtra)
#' pdr.nlme = nlme(PDR~BluckCoward(time,100,m,k,beta),data=pdr1,
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

BluckCoward = function(time,Dose,m,k,beta){
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

#' @name BluckCoward2
#' @title Equation 2 from Bluck Review
#'
#' @param Time in minutes
#' @param Dose in mg
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return vector of predicted cumulative PDR
#' @export
#' @seealso \code{\link{BluckCoward}}
BluckCoward2  = function(Time,Dose,cf){
  ekt = 1-exp(-cf["k"]*Time)
  beta = cf["beta"]
  Dose*(beta*(ekt)^(beta-1)-(beta-1)*ekt^beta)
}

#' @name t50BluckCoward2
#' @title Newton's method to solve BluckCoward2 for 1/2 to compute t_50
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{BluckCoward}}
#' @export
t50BluckCoward2 = function(cf){
  ret = round(uniroot(function(t) {
    BluckCoward2(t,1,cf)-0.5
  },c(1,1000))$root,1)
  names(ret)="t50BluckCoward"
  ret
}

#' @name tLagBluckCoward
#' @title Lag phase for BluckCoward self-correcting fit
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return lag phase in minutes (time t at which the maximum in the rate of change 
#' of G(t) occurs)
#' @seealso \code{\link{BluckCoward}}
#' @export
tLagBluckCoward = function(cf){
  ret = log(cf["beta"]/2)/cf["k"]  
  names(ret)="tLagBluckCoward"
  ret
}

#' @name t50Ghoos
#' @title Determine t50 the original way (Sanaka Nakada eq 6)
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{BluckCoward}}
#' @export
t50Ghoos = function(cf){
  ret = -log(1-2^(-1/cf["beta"]))/cf["k"]
  names(ret)="t50Ghoos"
  ret
}

#' @name tLagGhoos
#' @title Determine tlag from Ghoos formula
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return lag time as defined from Ghoos fit
#' @seealso \code{\link{BluckCoward}}
#' @export
tLagGhoos = function(cf){
  ret = log(cf["beta"])/cf["k"]
  names(ret)="tlagGhoos"
  ret
}

#' @name t50GhoosScintigraphy
#' @title t50 from Ghoos with scintigrapic correction
#' @description Computes t50 the original Ghoos way (Sanaka Nakada eq 6),
#' and apply the correction from Ghoos et. al 1993 to come close to 
#' scintigraphic values. This is for comparison with published data only;
#' there is little justification for using it.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{BluckCoward}}
#' @export
t50GhoosScintigraphy = function(cf){
  ret = (t50Ghoos(cf)-66.09)/1.12
  names(ret)="t50Ghoos"
  ret
}
