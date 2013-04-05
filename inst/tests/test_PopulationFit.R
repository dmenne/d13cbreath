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
  # Make one stupid outlier
  pp = pd$PDR[pd$BreathTestRecordID=="1"]
  pp = pp+(1:length(pp))*0.5
  pd$PDR[pd$BreathTestRecordID==1] = pp
  cf = BreathTestPopulationFit(pd)  
  expect_equal(nrow(cf),9)
  expect_equal(names(cf),c("BreathTestRecordID","m","k","beta"))
  sp = SavePopulationFit(cf,con)
  expect_is(sp,"data.frame")
  tb = as.numeric(table(sp$status))
  expect_equal(tb, c(9,1)) # 9 kept, 1 removed
})


dbDisconnect(con)
