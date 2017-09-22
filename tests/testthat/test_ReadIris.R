context("13C read IRIS test")

test_that("ReadIris returns valid data set",{
  filename = d13File("IrisMulti.TXT")
  f = ReadIris(filename)
  expect_is(f,"BreathTestData")
  expect_equal(f$FileName,basename(filename))
  expect_equal(f$Name,"V")
  expect_equal(f$FirstName,"S")
  expect_equal(f$Initials,"VS")
  expect_equal(f$PatientID,"1871960")
  expect_equal(nrow(f$Data),14)
  expect_equal(ncol(f$Data),3)
})

test_that("ReadIris returns valid data set when values are negative",{
  filename = d13File("IrisNegativeValues.TXT")
  f = ReadIris(filename)
  expect_is(f,"BreathTestData")
  expect_equal(nrow(f$Data),12)
  expect_equal(ncol(f$Data),3)
  expect_true(all(f$Data$DOB >= -10))
})

test_that("ReadIris returns valid data set when Weight/Height is zero",{
  filename = d13File("IrisZeroWeight.TXT")
  f = ReadIris(filename)
  expect_is(f,"BreathTestData")
  expect_true(is.na(f$Weight))
  expect_true(is.na(f$Height))
  expect_equal(nrow(f$Data),14)
  expect_equal(ncol(f$Data),3)
})


test_that("ReadIris of CSV file throws",{
  filename = d13File("IrisCSV.TXT")
  expect_error( ReadIris(filename),"valid Iris")
})

test_that("Missing substrate should give a useful error message",{
  filename = d13File("Iris_Missing_Substrate.TXT")
  expect_error(ReadIris(filename), "should contain substrings")
})


