context("Database write test")

test_that("Writing sample BreathID database returns valid set of fit parameters ",{
  if (exists("con")) suppressWarnings(dbDisconnect(con))
  sqlitePath = tempfile(pattern = "Gastrobase", tmpdir = tempdir(), fileext = ".sqlite")
  unlink(sqlitePath)
  CreateBreathTestDatabase(sqlitePath)
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
