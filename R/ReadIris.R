#' @title Read 13C data from IRIs/Wagner Analysen
#' 
#' @description Reads 13C data from IRIS/Wagner Analysen. Currently
#' it only support "standard" files in csv-format.
#' 
#' @param filename Name of IRIS/Wagner file in csv format
#' @return \code{readContrasts} 
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' filename = system.file("extdata", "standard.txt", package = "D13CBreath")
#' irisData = ReadIris(filename)
#' str(irisData)
#' @export

ReadIris = function(filename) {
  if (!file.exists(filename)) 
    stop(str_c("File ",filename," does not exist."))
  iris = try(read.csv(filename),silent=TRUE)
  if (class(iris)=="try-error")
    stop(str_c("Error reading file ",filename) )
  valid = all(names(iris)==c("Name","Vorname","Test","Identifikation",
    "Testzeit.min.","DOB..o.oo.","Delta..o.oo.","Std..Abw..o.oo.",
    "CO2....","Std..Abw....","Atom.ppm.Excess.13C..ppm.","Datum","Zeit"))
  if (!valid)  
    stop(str_c("The header row of ",basename(filename)+" has an unexpected format"))
  lapply(c("Name","Vorname","Test","Identifikation"), function(x){
    levs = as.factor(iris[,x])
    if (nlevels(levs) != 1)
      stop(str_c("There is more than one ",x," in file ",basename(filename),"\n",
            paste(levels(levs),collapse=", ")))
    invisible(NULL)
    })
  
  data = iris[,c(5,6)]
  names(data)=c("Time","DOB")
  data$Time = as.numeric(data$Time)
  structure(list(FileName=basename(filename),
       PatientName = as.character(iris[1,"Name"]),
       PatientFirstName = as.character(iris[1,"Vorname"]),
       Test = as.character(iris[1,"Test"]),
       Identifikation = as.character(iris[1,"Identifikation"]),     
       Data = data),class="irisData")
  
}


