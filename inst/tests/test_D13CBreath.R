context("13C read test")
filename = system.file("extdata", "standard.txt", package = "D13CBreath")


test_that("ReadIris returns valid data set",{
  f = ReadIris(filename)
  expect_equal(f,filename)
})

