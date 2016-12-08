context("Test of decision plot")

test_that("Decision plots must be generated",{
  databasePath = CreateSimulatedBreathTestDatabase()
  con = OpenSqliteConnection(databasePath)
  opt = par(mfrow = c(2,2))
  png_file = tempfile(fileext = ".png")
  png(png_file)
  DecisionPlot(con, showColors = "red", main = "Method ash")  
  dev.off()
  expect_true(file.exists(png_file))
  unlink(png_file)
})
  