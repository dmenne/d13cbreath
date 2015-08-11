#NAMESPACE <- environment()

.onLoad <- function(libname, pkgname) {
  options(Gastrobase2SqlitePath = file.path(Sys.getenv("HOME"),
                                            "Gastrobase2/Gastrobase2.sqlite"))
  options(D13CBreath.sqldebug = FALSE)
}
