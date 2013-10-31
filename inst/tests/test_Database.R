context("Database write test")


test_that("Writing sample BreathID database returns valid set of fit parameters ",{
  if (exists("con")) suppressWarnings(try(dbDisconnect(con)))
  sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateEmptyBreathTestDatabase(sqlitePath)
  con = OpenSqliteConnection(sqlitePath)
  filename = system.file("extdata", "350_20043_0_GER.txt", package = "D13CBreath")
  AddBreathTestRecord(filename,con)
  # Check Patient record
  pat = dbGetQuery(con,"SELECT * from Patient")
  expect_equal(pat$PatientID,"0")
  expect_equal(pat$Gender,"m")
  rec = dbGetQuery(con,"SELECT * from BreathTestRecord")
  expect_equal(nrow(rec),1)
  expect_equal(rec$TestNo,20043)
  expect_equal(rec$FileName,"350_20043_0_GER.txt")
  expect_equal(rec$PatientID,"0")
  # Check parameter record
  nParameters = dbGetQuery(con,"SELECT Parameter, Method from BreathTestParameter")
  
  dbDisconnect(con)
  unlink(sqlitePath)
  # BreathID, ExpBeta, BluckCoward, Maes, MaesScint,WN
  expect_equal(length(unique(nParameters$Method)),6,
               info = paste(unique(nParameters$Method),collapse=", "))    
  # t50 tlag GEC beta deviance k m       
  expect_equal(length(unique(nParameters$Parameter)),7,
               info = paste(unique(nParameters$Parameter),collapse=", "))
})

test_that("Update Cascade and Delete Cascade must be effective for BreathTestRecord",{
  countSQL = function(PatientID){
    sprintf(
      "SELECT Count(*) from BreathTestRecord where  patientID='%s'",PatientID)
  }
  sqlitePath = CreateSimulatedBreathTestDatabase()
  con = OpenSqliteConnection(sqlitePath)
  patID = dbGetQuery(con,"SELECT PatientID from Patient")$PatientID
  # Test Delete
  expect_equal(dbGetQuery(con, countSQL(patID[1]))[1,1] ,3)
  dbSendQuery(con, sprintf("DELETE from Patient where patientID='%s'",patID[1]))
  expect_equal(dbGetQuery(con, countSQL(patID[1]))[1,1] ,0)
  # Test Update
  expect_equal(dbGetQuery(con, countSQL(patID[2]))[1,1] ,3)
  dbSendQuery(con, 
    sprintf("UPDATE Patient SET patientID='blub' where PatientID='%s'", patID[2]))
  expect_equal(dbGetQuery(con, countSQL(patID[2]))[1,1] ,0)
  expect_equal(dbGetQuery(con, countSQL('blub'))[1,1] ,3)  
  dbDisconnect(con)
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
 expect_equal(length(sum$Record),23)
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
  #### Change this when additional parameters are added
  expectParams = c("CPDR","CPDRfit","DOB","PDR","PDRfit","WN")
  expect_equal(pars[,1],expectParams)
  # !!! Change this if additional test are added
  ExpectUnique = c(NA,1,2,3,4,5)
  ExpectTab = c(5,5)
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
  ### Change this !!
  expectParams = c("CPDR","DOB","PDR","PDRfit","WN")
  expect_equal(pars[,1],expectParams)
  dbDisconnect(con)
  unlink(sqlitePath)
})

test_that("Wagner-Nelson creates a valid predicted time series",{
   sqliteFile = CreateSimulatedBreathTestDatabase()
   con = OpenSqliteConnection(sqliteFile)
   BreathTestRecordID = 1
   wn = dbGetQuery(con,"SELECT * from BreathTestParameter where Method = 'WN'")
   expect_equal(nrow(wn),10)
   expect_true(max(wn$Value) < 50)
   expect_true(min(wn$Value) > 10)
   ts = dbGetQuery(con,"SELECT * from BreathTestTimeSeries where Parameter = 'WN'")
   expect_equal(range(ts$Time),c(0,225))
   expect_true(max(ts$Value)<=1)
   expect_true(min(ts$Value)>=-0.2)
   dbDisconnect(con)
})

