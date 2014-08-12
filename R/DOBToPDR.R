#' @title Convert DOB to PDR 
#' 
#' @description Convert DOB to PDR for 13C breath test. This is equation (4) in 
#' Sanaka, Yamamoto, Tsutsumi, Abe, Kuyama (2005) Wagner-Nelson method for analysing 
#' the atypical double-peaked [13CO2] excretion curve in the [13C]-octanoate gastric
#' emptying breath test in humans. Clinical and Experimental Pharmacology and 
#' Physiology 32, 590-594.
#' @note I have no idea where the factor 10 in equation (4) comes from, possibly
#' from percent(PDR)/and DOB(0/00). In Kim and 
#' Camillieri, Stable Isotope Breath Test and Gastric Emptying, page 207 a factor
#' of 0.1123 instead of 0.01123 is used, without the factor 10. Which one is correct?
#' 
#' @param  DOB vector in 0/00
#' @param  weight body weight in kg; assumed 75 kg if missing
#' @param  height body height in cm; assume 180 cm if missing
#' @param  MW Molecular weight,  83.023388 g/mol for acetate, 167 g/mol for octanoate.
#' Can also be given as string "acetate" or "octanoate".
#' @param  purityPercent purity in percent
#' @param  mgSubstrate substrate in mg
#' @return PDR percent dose/h
#' @examples
#' filename = system.file("extdata/extrasample", "350_20049_0_GERWithWeight.txt", package = "D13CBreath")
#' bid = ReadBreathId(filename)
#' bid$Data$PDR1 = DOBToPDR(bid$Data$DOB,weight=bid$Weight,height=bid$Height)
#' 
#' plot(bid$Data$Time, bid$Data$PDR1,main="Points: From BreathID; line: computed",type="l")
#' points(bid$Data$Time, bid$Data$PDR,col="red",type="p",pch=16)
#' var(bid$Data$PDR1-bid$Data$PDR)
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @export
DOBToPDR = function(DOB,weight=75,height=180,MW=167,purityPercent=99.1,
                      mgSubstrate=100){
  if (is.na(weight) || is.null(weight) || max(weight)<20 ) weight=75
  if (is.na(height) || is.null(height) ) height=180
  if (max(height) < 2 ) height = height*100 # Correct for people giving height in meter
  if (is.character(MW))  {
    if (MW =="octanoate") MW = 167 else
    if (MW == "acetate") MW = 83.0233388 else
    stop("DOBToPDR: MW must be 'octanoate' or 'acetate' or numeric")
  }
  surface= 0.024265* weight^0.5378*height^0.3964
  CO2PerMinute = 300*surface # 
  rpdb = 0.0112372 # isotope ratio in  reference
  # See above note on 
  DOB*CO2PerMinute*rpdb*10*MW/(mgSubstrate*purityPercent)
}


