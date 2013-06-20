#' @name t50BluckCoward
#' @title Self-corrected t_{50}
#' @description
#' Newton's method to solve the self-corrected BluckCoward equation 
#' for 1/2 to compute t_50.
#' See also equation G(n,t) in "Bluck LJC, Jackson S, Vlasakakis G, Mander A (2011) 
#' Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath 
#' test for gastric emptying. Digestion 83_96-107, page 98.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' Note that in this package, \code{k} is measured in 1/min (e.g. 0.01/min), 
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return time where value is 1/2 of maximum, i.e. t_{50} or t_{1/2} in minutes; in the above
#' paper, the parameter is called t_{1/2(in)}.
#' @examples
#' # From table 3 and 4 in Bluck et al.; values for k and beta(NLS, Bayesian) are entered
#' # and checked against the tabulated values of t_{1/2(in)}.
#' # Most errors are small, but there are some outliers; errors in paper table?
#' # Parameters and Bluck results:
#' library(plyr) # for mutate
#' # Table 3 of Bluck et al.
#' cf3 = data.frame(
#'           method = rep(c("NLS","Bayesian")),
#'           group = rep(c("Lean","Obese"),each=2),
#'           k =    c(0.576,0.606,0.529,0.608), 
#'           beta = c(5.24, 5.79, 5.95, 7.54),
#'           t12 =  c(3.67, 3.63, 4.23, 3.99),
#'           t12in = c(2.076,2.110,2.422,2.466),
#'           tlag = c(2.88,2.88,3.34,3.26),
#'           tlagin = c(1.632, 1.724,1.92,2.101)
#' )
#' cf3 = mutate(cf3,
#'           t50Maes = t50Maes(cf3),
#'           t50BluckCoward=t50BluckCoward(cf3),
#'           tLagMaes = tLagMaes(cf3),
#'           tLagBluckCoward = tLagBluckCoward(cf3),
#'           Err_t50Maes = round(100*(t50Maes-t12)/t12,2),
#'           Err_t50BluckCoward = 
#'             round(100*(t50BluckCoward-t12in)/t12in,2),
#'           Err_LagMaes = round(100*(tLagMaes-tlag)/tlag,2),
#'           Err_LagBluckCoward = 
#'             round(100*(tLagBluckCoward-tlagin)/tlagin,2)
#')
#' cf3
#' # Table 4
#' # There are large differences for MJ3, both using the Bayesian (26%)
#' # and the NLS method (16%).  The other data are within the expected limits
#' cf4 = data.frame(
#'           method = rep(c("NLS","Bayesian"),each=3),
#'           group = rep(c("MJ1","MJ2","MJ3")),
#'           k = c(0.585,0.437,0.380,0.588,0.418,0.361), 
#'           beta=c(4.35,4.08,4.44,4.49,4.30,4.29),
#'           t12 = c(3.39,4.25,4.82,3.40,4.61,5.09),
#'           t12in = c(1.77,2.16,2.19,1.81,2.34,2.43),
#'           tlag = c(2.56,3.17,3.39,2.58,3.40,3.62),
#'           tlagin = c(1.30,1.53,1.33,1.35,1.65,1.57)
#' )
#' cf4 = mutate(cf4,
#'           t50Maes = t50Maes(cf4),
#'           t50BluckCoward=t50BluckCoward(cf4),
#'           tLagMaes = tLagMaes(cf4),
#'           tLagBluckCoward = tLagBluckCoward(cf4),
#'           Err_t50Maes = unlist(round(100*(t50Maes-t12)/t12)),
#'           Err_t50BluckCoward = 
#'             round(100*(t50BluckCoward-t12in)/t12in,2),
#'           Err_LagMaes = round(100*(tLagMaes-tlag)/tlag,2),
#'           Err_LagBluckCoward = 
#'             round(100*(tLagBluckCoward-tlagin)/tlagin,2)
#')
#' cf4
#' 
#' #' @seealso \code{\link{ExpBeta}}
#' @export
t50BluckCoward = function(cf){
  f = function(t,cf0) CumExpBeta(t,1,cf0)-0.5
  g = function(cf0){
    uniroot(f,interval= c(1,1000),cf0)$root
  }
  if (class(cf)=="numeric")
    round(g(cf),3)
  else 
    round(apply(cf[,c("k","beta")],1,g),3)
}

#' @name tLagBluckCoward
#' @title Lag phase for BluckCoward self-correcting fit
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' Note that in D13CBreath, \code{k} is measured in 1/min (e.g. 0.01/min), 
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return lag phase in minutes (time t at which the maximum in the rate of change 
#' of G(t) occurs)
#' @seealso \code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}} for an example.
#' @export
tLagBluckCoward= function(cf){
  unlist(log(cf["beta"]/2)/cf["k"])
}

#' @name t50Maes
#' @title t50 as determined from an uncorrected fit to the beta exponential function.
#' @description
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' Note that \code{k} is measured in 1/min (e.g. 0.01/min), 
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}} for an example.
#' @export
t50Maes = function(cf){
  unlist(-log(1-2^(-1/cf["beta"]))/cf["k"])
}

#' @name tLagMaes
#' @title Determine tlag from uncorrected fit to the beta exponential function
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' Note that \code{k} is measured in 1/min (e.g. 0.01/min), 
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return lag time as defined from Maes fit
#' @seealso \code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}} for an example.
#' @export
tLagMaes = function(cf){
  unlist(log(cf["beta"])/cf["k"])
}

#' @name t50MaesScintigraphy
#' @title t50 from Maes with scintigrapic correction
#' @description t50 from beta exponential function, with linear correction for 
#' scintigraphic values. This is for comparison with published data only;
#' there is little justification for using it.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes
#' @seealso \code{\link{ExpBeta}}, and \code{\link{t50BluckCoward}} for an example.
#' @export
t50MaesScintigraphy = function(cf){
  (t50Maes(cf)-66.09)/1.12
}
