context("Plot test")

test_that("PlotRecord should not fail",{
  sqlitePath = CreateSimulatedBreathTestDatabase()
  con = OpenSqliteConnection(sqlitePath)
  showParameters = data.frame(Parameter = "t50",Method = c("BreathID","BluckCoward"))
  expect_silent(Plot13CRecord(con, 1))
  expect_silent(Plot13CRecord(con, 1, showParameters))
  dbDisconnect(con)
})