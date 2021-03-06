context("Test of extraction of BreathID files from zip file ")

test_that("Test ok cases during file extraction without date comparison",{
  # Prepare zip file and directories
  tmpdir = tempdir()
  zipPath = file.path(tmpdir,"zipped")
  suppressWarnings(dir.create(zipPath))
  destinationPath = file.path(tmpdir,"zipdestination")
  suppressWarnings(dir.create(destinationPath))
  unlink(file.path(destinationPath,"*.txt"))
  zipFile = system.file("extdata/zipExample", "BreathIdZipExample.zip", package = "D13CBreath")
  file.copy(zipFile,zipPath)
  
  # Test normal case without dates of old file
  ret = UnzipBreathID(zipPath,destinationPath)
  expect_equal(ret$n, 3)
  expect_equal(ret$type, "ok")
  expect_match(ret$msg, "New")
  expect_true(file.exists(ret$lastZipFile))
  expect_equal(ret$lastZipDate, as.character(file.info(ret$lastZipFile)$mtime))
  firstFile = file.path(destinationPath,ret$files[1])
  # Try again, nothing should be extracted
  ret = UnzipBreathID(zipPath,destinationPath,inZipPath = "txt/")
  expect_equal(ret$n, 0)
  expect_equal(ret$type, "ok")
  expect_match(ret$msg, "All")
  expect_true(file.exists(ret$lastZipFile))
  expect_equal(ret$lastZipDate, as.character(file.info(ret$lastZipFile)$mtime))
  # Remove one file, extract again
  file.remove(firstFile)
  expect_true(!file.exists(firstFile))
  ret = UnzipBreathID(zipPath,destinationPath,inZipPath = "txt/")
  expect_equal(ret$n, 1)
  expect_equal(ret$type, "ok")
  expect_match(ret$msg, "New")
  expect_true(file.exists(firstFile))
  expect_true(file.exists(ret$lastZipFile))
  expect_equal(ret$lastZipDate, as.character(file.info(ret$lastZipFile)$mtime))
  
  unlink(zipPath,TRUE)
  unlink(destinationPath,TRUE)
})


test_that("Test ok cases during file extraction with date comparison",{
  # Prepare zip file and directories
  tmpdir = tempdir()
  zipPath = file.path(tmpdir,"zipped")
  suppressWarnings(dir.create(zipPath))
  destinationPath = file.path(tmpdir,"zipdestination")
  suppressWarnings(dir.create(destinationPath))
  unlink(file.path(destinationPath,"*.txt"))
  zipFile = system.file("extdata/zipExample", "BreathIdZipExample.zip", package = "D13CBreath")
  file.copy(zipFile,zipPath)
  lastZipFile = dir(zipPath,"*.zip",full.names = TRUE)[1]
  lastZipDate = file.info(lastZipFile)$mtime
  
  # Unzip all
  ret = UnzipBreathID(zipPath,destinationPath)
  expect_that(ret$n,equals(3))
  firstFile = file.path(destinationPath,ret$files[1])
  # Make one file older
  Sys.setFileTime(firstFile, "2000-01-01")
  ret = UnzipBreathID(zipPath,destinationPath)
  # it should be extracted
  expect_equal(ret$n, 1)
  # If no date given, it should unpack
  unlink(file.path(destinationPath,"*.txt"))
  ret = UnzipBreathID(zipPath,destinationPath,lastZipFile = lastZipFile)
  expect_equal(ret$n, 3)
  # If date given, it should not unpack
  #  unlink(file.path(destinationPath,"*.txt"))
  ret = UnzipBreathID(zipPath,destinationPath,
                      lastZipFile, lastZipDate)
  expect_equal(ret$n, 0)
  expect_match(ret$msg, "processed")
  # If date given, it should not unpack, even if the target files do not exist
  ### This is risky....
  unlink(file.path(destinationPath,"*.txt"))
  ret = UnzipBreathID(zipPath,destinationPath,lastZipFile = lastZipFile,
                      lastZipDate = lastZipDate)
  expect_equal(ret$n, 0)
  expect_match(ret$msg, "already")
  # Setting date to older forces read
  unlink(file.path(destinationPath,"*.txt"))
  ret = UnzipBreathID(zipPath,destinationPath,lastZipFile = lastZipFile,
                      lastZipDate = "2000-01-01")
  expect_equal(ret$n, 3)
  expect_equal(ret$type, "ok")
  expect_match(ret$msg, "New files")
  unlink(zipPath,TRUE)
  unlink(destinationPath,TRUE)
})

test_that("Test error and info cases",{
  # Prepare zip file and directories
  tmpdir = tempdir()
  zipPath = file.path(tmpdir,"zipped")
  suppressWarnings(dir.create(zipPath))
  unlink(file.path(zipPath,"*.zip"))
  destinationPath = file.path(tmpdir,"zipdestination")
  suppressWarnings(dir.create(destinationPath))
  unlink(file.path(destinationPath,"*.txt"))
  zipFile = system.file("extdata/zipExample", "BreathIdZipExample.zip", package = "D13CBreath")
  lastZipFile = dir(zipPath,"*.zip",full.names = TRUE)[1]
  lastZipDate = file.info(lastZipFile)$mtime
  
  # No zip file
  ret = UnzipBreathID(zipPath,destinationPath,lastZipFile = lastZipFile,
                      lastZipDate = "2000-01-01")
  expect_equal(ret$n, 0)
  expect_equal(ret$type, "info")
  expect_match(ret$msg, "No zip")
  # Multiple zip files
  file.copy(zipFile,zipPath)
  file.copy(zipFile,file.path(zipPath,"a.zip"))
  ret = UnzipBreathID(zipPath,destinationPath)
  expect_equal(ret$type, "error")
  expect_match(ret$msg, "More than")
  ret = UnzipBreathID(zipPath,destinationPath,lastZipFile = lastZipFile,
                      lastZipDate = "2000-01-01")
  expect_equal(ret$type, "error")
  expect_match(ret$msg, "More than")
  # Non-existing directory (typically USB stick)
  badZipPath = "X:"
  if (!file_test("-d",paste0(badZipPath,"/.")))
  {
    ret = suppressWarnings(UnzipBreathID(badZipPath,destinationPath))
    expect_equal(ret$n, 0)
    expect_equal(ret$type, "info")
    expect_match(ret$msg, "Zip source")
  }
  # Cleanup
  unlink(zipPath,TRUE)
  unlink(destinationPath,TRUE)
  
})  