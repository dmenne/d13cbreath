library(testthat)
#library(D13CBreath)
library(stringr)
options(warn=2)
d13File = function(filename){
  system.file("extdata", filename, package = "D13CBreath")  
}

test_check("D13CBreath")
