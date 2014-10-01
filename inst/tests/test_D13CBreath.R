context("13C read test")
d13File = function(filename){
  system.file("extdata", filename, package = "D13CBreath")  
}

test_that("ReadIris returns valid data set",{
  filename = d13File("IrisMulti.txt")
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

test_that("ReadIris of CSV file throws",{
  filename = d13File("IrisCSV.txt")
  expect_error( ReadIris(filename),"valid Iris")
})
          
test_that("ReadBreathID returns valid data set",{
  breathfilename = d13File("350_20043_0_GER.txt")
  f = ReadBreathId(breathfilename)
  expect_is(f,"BreathTestData")
  expect_equal(f$FileName,basename(breathfilename))
  expect_equal(f$TestNo,20043)
  expect_equal(f$T50,71.23)
  expect_equal(f$PatientID,"0")
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
  expect_is(f,"BreathTestData")
  expect_true(!"CPDRfit" %in% names(f$Data))
} )

test_that("DOBToPDR is not too far from what BreathID says",{
  filename = system.file("extdata/extrasample", "350_20049_0_GERWithWeight.txt", package = "D13CBreath")
  bid = ReadBreathId(filename)
  bid$Data$PDR1 = DOBToPDR(bid$Data$DOB,weight=bid$Weight,height=bid$Height)
  expect_true(sqrt(var(bid$Data$PDR1-bid$Data$PDR))<0.032)
})
