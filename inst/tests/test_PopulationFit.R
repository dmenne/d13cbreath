context("Population fit test")
set.seed(4711)
sqlitePath = CreateSimulatedBreathTestDatabase()
con = OpenSqliteConnection(sqlitePath)


test_that("Data can be read for population fit",{
  pd = GetPopulationData(con)  
  expect_equal(nrow(pd),860)
  expect_equal(names(pd),c("BreathTestRecordID","Time","PDR"))
})

test_that("Population fit can be computed and written to database",{
  pd = GetPopulationData(con)  
  cf = BreathTestPopulationFit(pd)  
  expect_equal(nrow(cf),10)
  expect_equal(names(cf),c("BreathTestRecordID","m","k","beta"))
  sp = SavePopulationFit(cf,con)
  expect_is(sp,"data.frame")
  expect_equal(levels(sp$status), "kept")
})


dbDisconnect(con)
