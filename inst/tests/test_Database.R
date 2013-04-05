context("Database write test")

test_that("Writing sample BreathID database returns valid set of fit parameters ",{
  if (exists("con")) suppressWarnings(dbDisconnect(con))
  sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
  AddBreathTestRecord(filename,con)
  nParameters = dbGetQuery(con,"SELECT Parameter, Method from BreathTestParameter")
  
  dbDisconnect(con)
  unlink(sqlitePath)
  expect_equal(length(unique(nParameters$Method)),4,
               info = paste(unique(nParameters$Method),collapse=", "))           
  expect_equal(length(unique(nParameters$Parameter)),6,
               info = paste(unique(nParameters$Parameter),collapse=", "))
})


test_that("Summary returns list of Record and Parameters",{
 sqlitePath = CreateSimulatedBreathTestDatabase()
 con = OpenSqliteConnection(sqlitePath)
 breathTestRecordID=1
 sum = Summary13CRecord(con,breathTestRecordID)
 dbDisconnect(con)
 expect_equal(length(sum),2)
 expect_equal(class(sum[[1]]),"data.frame")
 expect_equal(class(sum[[2]]),"data.frame")
 expect_equal(length(sum$Record),22)
 expect_that( nrow(sum$Parameters)>5,is_true())
})


test_that("Reading of multiple files returns dataframe with status",{
  # Setup
  if (exists("con")) suppressWarnings(dbDisconnect(con))
  sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  path = dirname(
    system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath"))
  # Test
  res = AddAllBreathTestRecords(path,con)
  res1 = AddAllBreathTestRecords(path,con) # Try again, same records
  pars = dbGetQuery(con,"SELECT DISTINCT Parameter from BreathTestTimeSeries order by Parameter") 
  dbDisconnect(con)
  unlink(sqlitePath)
  # Assert
  tab = table(res$status)
  tab1 = table(res1$status)
  expect_equal(pars[,1],c("CPDR","CPDRfit","DOB","PDR","PDRfit"))
  # !!! Change this if additional test are added
  ExpectUnique = c(NA,1,2,3)
  ExpectTab = c(6,3)
  expect_true(all(unique(res$recordID) %in% ExpectUnique))
  expect_equal(names(tab),c("invalid","saved"))
  expect_equal(names(tab1),c("invalid","skipped"))
  expect_equal(as.integer(tab),ExpectTab)
  expect_equal(as.integer(tab1),ExpectTab)
})

test_that("Data columns with NaN are not stored",{
  # Setup
  if (exists("con")) suppressWarnings(dbDisconnect(con))
  sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  filename = system.file("extdata", "350_20023_0_GERWithNan.txt", 
                         package = "D13CBreath")
  AddBreathTestRecord(filename,con)
  pars = dbGetQuery(con,"SELECT DISTINCT Parameter from BreathTestTimeSeries order by Parameter") 
  expect_equal(pars[,1],c("CPDR","DOB","PDR","PDRfit"))
  dbDisconnect(con)
  unlink(sqlitePath)
})

