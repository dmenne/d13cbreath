library(testthat)
library(D13CBreath)
library(stringr)
d13File = function(filename){
  system.file("extdata", filename, package = "D13CBreath")  
}

test_package("D13CBreath")
