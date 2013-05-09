#' @name BluckCoward2
#' @title Bluck-Coward self-correcting t50 from 13C breath test
#' @description Equation (2), page 4 from Bluck, "Recent advances in the interpretation of
#' the 13C octanoate breath test for gastric emptying", solved for time of 50%
#'
#' @param Time in minutes
#' @param Dose in mg
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return vector of predicted cumulative PDR
#' @export
#' @seealso \code{\link{ExpBeta}}
BluckCoward2  = function(Time,Dose,cf){
  ekt = 1-exp(-unlist(cf["k"])*Time)
  beta = unlist(cf["beta"])
  as.numeric(Dose*(beta*(ekt)^(beta-1)-(beta-1)*ekt^beta))
}

#' @name t50BluckCoward
#' @title Newton's method to solve the self-corrected BluckCoward equation 
#' for 1/2 to compute t_50
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{ExpBeta}}
#' @export
t50BluckCoward = function(cf){
  f = function(t,cf0) BluckCoward2(t,1,cf0)-0.5
  g = function(cf0){
    uniroot(f,interval= c(1,1000),cf0)$root
  }
  if (class(cf)=="numeric")
    round(g(cf),1)
  else 
    data.frame(t50BluckCoward=round(apply(cf[,c("k","beta")],1,g),1))
}

#' @name tLagBluckCoward
#' @title Lag phase for BluckCoward self-correcting fit
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return lag phase in minutes (time t at which the maximum in the rate of change 
#' of G(t) occurs)
#' @seealso \code{\link{ExpBeta}}
#' @export
tLagBluckCoward= function(cf){
  ret = log(cf["beta"]/2)/cf["k"]  
  names(ret)="tLagBluckCoward"
  ret
}

#' @name t50Maes
#' @title t50 as determined from an uncorrected fit to the beta exponential function.
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{ExpBeta}}
#' @export
t50Maes = function(cf){
  ret = -log(1-2^(-1/cf["beta"]))/cf["k"]
  names(ret)="t50Maes"
  ret
}

#' @name tLagMaes
#' @title Determine tlag from uncorrected fit to the beta exponential function
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return lag time as defined from Maes fit
#' @seealso \code{\link{ExpBeta}}
#' @export
tLagMaes = function(cf){
  ret = log(cf["beta"])/cf["k"]
  names(ret)="tlagMaes"
  ret
}

#' @name t50MaesScintigraphy
#' @title t50 from Maes with scintigrapic correction
#' @description t50 from beta exponential function, with linear correction for 
#' scintigraphic values. This is for comparison with published data only;
#' there is little justification for using it.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{ExpBeta}}
#' @export
t50MaesScintigraphy = function(cf){
  ret = (t50Maes(cf)-66.09)/1.12
  names(ret)="t50MaesScintigraphy"
  ret
}
