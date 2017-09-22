context("13C read IRIS CSV test")

test_that("ExtractID returns valid id" , {
  expect_equal(ExtractID("123456"), "123456")  
  expect_equal(ExtractID("123-456"), "123_456")  
  expect_equal(ExtractID("KEK-ZH-Nr.2013-1234"), "2013_1234")
  expect_equal(ExtractID("Las4Dd5 .f lkj"), "las4dd5_f_lkj")
})

test_that("ReadIrisCSV returns valid data set",{
  filename = d13File("IrisCSV.TXT")
  f = ReadIrisCSV(filename)
  expect_is(f,"BreathTestData")
  expect_equal(f$FileName,basename(filename))
  expect_equal(f$Name,"Einstein")
  expect_equal(f$FirstName,"Albert")
  expect_equal(f$Initials,"EA")
  expect_equal(f$PatientID,"123456")
  expect_equal(nrow(f$Data),14)
  expect_equal(ncol(f$Data),3)
  expect_equal(f$Study, "GE FEST")
})

test_that("ReadIrisCSV returns with funny identification cleans up",{
  filename = d13File("IrisCSV_with_KEK.TXT")
  f = ReadIrisCSV(filename)
  expect_equal(f$PatientID,"2013_1234")
  # Works interactively, but fails on Build under Windows
  if (.Platform$OS.type != 'windows')
    expect_equal(f$Study, "GE-flüssig")
})

test_that("ReadIrisCSV raises error on short file",{
  filename = d13File("IrisCSVShort.TXT")
  expect_error(ReadIrisCSV(filename), "has only 2 rows")
})

test_that("ReadIrisCSV raises error on invalid entries",{
  filename = d13File("IrisCSV_invalidValues.TXT")
  expect_error(ReadIrisCSV(filename), "2 parsing failures")
})


test_that("Iris CSV file can be added to database", {
  sqlitePath = tempfile(pattern = "Gastrobase", 
                        tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  filename = d13File("IrisCSV_with_KEK.TXT")
  expect_equal(AddIrisCSVRecord(filename,con), 1)
  dbDisconnect(con)
})
