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
  # Test all-good case
  cf = BreathTestPopulationFit(pd)  
  expect_equal(nrow(cf),10)
  
  # Make one stupid outlier
  pp = pd$PDR[pd$BreathTestRecordID=="1"]
  pp = pp+(1:length(pp))*0.5
  pd$PDR[pd$BreathTestRecordID==1] = pp
  cf = BreathTestPopulationFit(pd)  
  # The fit will be correctly includeds
  expect_equal(nrow(cf),9)
  expect_equal(names(cf),c("BreathTestRecordID","m","k","beta"))
  sp = SavePopulationFit(cf,con)
  expect_is(sp,"data.frame")
  tb = as.numeric(table(sp$status))
  expect_equal(tb, c(9,1)) # 9 kept, 1 removed
})

RandomizeRemoveNACoefficient = function(cf){
  r = D13CBreath:::RemoveNACoefficients(cf)  
  sample(r,length(r))
}

if (FALSE) # This test need long time to run and produces a lot of error messages
test_that("NLME fit must return the same results when nlsList invalid results are randomized",{
  # This test is only run when there is a default database with real data
  # The success of this test depends on actual data from measurements, not on simulated
  # data. In case it fails, it can be discarded, because the inclusion of results from 
  # non-successful nlsList runs anyway is not very promising.
  if (!file.exists(getOption("Gastrobase2SqlitePath"))) 
    return();
  set.seed(4711)
  RemoveItemsFunction=RandomizeRemoveNACoefficient
  x1 = BreathTestPopulationFit(NULL,RemoveItemsFunction=RandomizeRemoveNACoefficient)
  x2 = BreathTestPopulationFit(NULL,RemoveItemsFunction=RandomizeRemoveNACoefficient)
  x3 = BreathTestPopulationFit(NULL,RemoveItemsFunction=RandomizeRemoveNACoefficient)
  ## This test may fail, because the method is not very reliable
  expect_equal(x1$m,x2$m)
  expect_equal(x1$m,x3$m)
})

dbDisconnect(con)
