#NAMESPACE <- environment()
#' @importFrom graphics arrows points text
#' @importFrom stats coef median na.omit nls predict rnorm runif smooth.spline uniroot update var
#' @importFrom utils file_test read.csv read.table unzip
.onLoad <- function(libname, pkgname) {
  options(Gastrobase2SqlitePath = file.path(Sys.getenv("HOME"),
                                            "Gastrobase2/Gastrobase2.sqlite"))
  options(D13CBreath.sqldebug = FALSE)
}
