context("Test of extraction of BreathID files from ")

test_that("Test ok cases during file extraction without date comparison",{
  # Prepare zip file and directories
  tmpdir = tempdir()
  zipPath = file.path(tmpdir,"zipped")
  suppressWarnings(dir.create(zipPath))
  destinationPath = file.path(tmpdir,"zipdestination")
  suppressWarnings(dir.create(destinationPath))
  zipFile = system.file("extdata/zipExample", "BreathIdZipExample.zip", package = "D13CBreath")  
  file.copy(zipFile,zipPath)  

  # Test normal case without dates of old file
  ret = UnzipBreathID(zipPath,destinationPath,inZipPath = "txt/")
  expect_that(ret$n,equals(3))
  expect_that(ret$type,equals("ok"))
  expect_that(ret$msg,matches("New"))
  firstFile = file.path(destinationPath,ret$files[1])
  # Try again, nothing should be extracted
  ret = UnzipBreathID(zipPath,destinationPath,inZipPath = "txt/")
  expect_that(ret$n,equals(0))
  expect_that(ret$type,equals("ok"))
  expect_that(ret$msg,matches("All"))
  # Remove one file, extract again
  file.remove(firstFile)
  expect_true(!file.exists(firstFile))
  ret = UnzipBreathID(zipPath,destinationPath,inZipPath = "txt/")
  expect_that(ret$n,equals(1))
  expect_that(ret$type,equals("ok"))
  expect_that(ret$msg,matches("New"))
  expect_true(file.exists(firstFile))

  unlink(zipPath)
  unlink(destinationPath)
})


