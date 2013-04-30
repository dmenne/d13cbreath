context("13C read test")

test_that("ReadIris returns valid data set",{
  filename = d13File("standard.txt")
  f = ReadIris(filename)
  expect_is(f,"irisData")
  expect_equal(f$FileName,basename(filename))
  expect_equal(f$PatientName,"RH1_1988")
  expect_equal(f$PatientFirstName,"Visit 1")
  expect_equal(f$Test,"LSNFN")
  expect_equal(f$Identifikation,"10350699")
  expect_equal(nrow(f$Data),26)
  expect_equal(ncol(f$Data),2)
})

test_that("ReadIris of invalid data throws",{
  filename = d13File("standardMultiName.txt")
  expect_error( ReadIris(filename),"more than one")
})
          
test_that("ReadBreathID returns valid data set",{
  breathfilename = d13File("350_20043_0_GER.txt")
  f = ReadBreathId(breathfilename)
  expect_is(f,"breathIdData")
  expect_equal(f$FileName,basename(breathfilename))
  expect_equal(f$TestNo,20043)
  expect_equal(f$T50,71.23)
  expect_equal(f$PatientNumber,"0")
  expect_equal(f$Gender,"m")
  expect_equal(nrow(f$Data),87)
  expect_equal(ncol(f$Data),6)
  expect_true("CPDRfit" %in% names(f$Data))
})

test_that("ReadBreathID on bad data file throws",{
  filename = d13File("350_20043_0_GERBadHeader.txt")
  expect_error(ReadBreathId(filename),"not a valid BreathID")
  filename = d13File("350_20043_0_GERNoData.txt")
  expect_error( ReadBreathId(filename),"does not contain PDR")
  filename = d13File("350_20043_0_GERNoT50.txt")
  expect_error( ReadBreathId(filename),"No <<T 1/2>> found")
} )

test_that("ReadBreathID with NaN returns valid data, without NaN Columns",{
  filename = d13File("350_20023_0_GERWithNan.txt")
  f = ReadBreathId(filename)
  expect_is(f,"breathIdData")
  expect_true(!"CPDRfit" %in% names(f$Data))
} )

