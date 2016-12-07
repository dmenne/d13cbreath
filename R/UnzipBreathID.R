#' @title Unpacks a BreathID zip-File
#'
#' @description Retrieves BreathID records from a zipped file, and copies these
#' to a target directory.
#'
#' @param zipPath path to the zip file; the name of the zip file is optional. For security, nothing
#' is read when more than one zip file is found in the directory. When a full path
#' to a file is given and \code{lastReadDate != NULL}, the file is skipped if
#' the its modification date is earlier.
#' @param destinationPath directory where to store the unpacked file
#' @param inZipPath partial string where to find the files in the zip directory tree.
#' For example, if the path is \code{temp/BreathID/Results/txt/},  \code{inzipPath="txt/"}
#' is sufficient.
#' @param lastZipFile file name without path of last zip file; if NULL, not checked
#' @param lastZipDate modification date of last zip read; if NULL; not checked
#' @return A list with number of files \code{n}, \code{type=c("error","info","ok")}, and
#' message string \code{msg}. If \code{ok}, also vector of files that were extracted,
#' the name of the \code{lastZipFile}, and the modification date \code{lastZipDate}.
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @export
UnzipBreathID = function(zipPath,destinationPath,
                         lastZipFile = NULL, lastZipDate = NULL, inZipPath =
                           "txt/") {
  if (FALSE) {
    #zipPath = "C:/Users/Dieter/Documents/Gastrobase2"
    zipPath = "G:"
    lastZipFile = "350_6_4_2013_21_27.zip"
    destinationPath = "C:\\tmp"
    inZipPath = "txt/"
    lastZipDate = NULL#"2013-06-05 15:03:55"
  }
  # Correct path of form G: (This has not be tested on NonWindows systems)
  zipPath1 = normalizePath(zipPath)
  destinationPath = normalizePath(destinationPath)
  if (str_detect(zipPath1,"^[A-Z]:\\\\*$"))
    # This fails on non-Windows
    zipPath1 = paste0(zipPath1,"/.")
  
  if (!is.null(lastZipDate))
    lastZipDate = as.POSIXct(lastZipDate)
  
  isExistingFile = file_test("-f",zipPath)
  if (isExistingFile) {
    zipFile = zipPath
  } else {
    # Assume it is a path
    zips = dir(zipPath,"*.zip",full.names = TRUE)
    # We make it info, to allow for removal of USB stick with taking this as an error
    if (!file_test("-d",zipPath1))
      return(list(
        n = 0, type = "info", msg = paste0("Zip source directory ", zipPath, " does not exist")
      ))
    if (!file_test("-d",destinationPath))
      return(list(
        n = 0, type = "error", msg = paste0(destinationPath, zipPath, " is not a directory")
      ))
    if (length(zips) == 0)
      return(list(
        n = 0, type = "info", msg = paste0("No zip file found in ",zipPath)
      ))
    if (length(zips) > 1)
      return(list(
        n = 0, type = "error", msg = paste0("More than 1 zip file found in ",zipPath),
        files = zips
      ))
    zipFile = zips[1]
  }
  # File is real, check if already processed
  mtime = file.info(zipFile)$mtime
  stopifnot(!is.na(mtime))
  alreadyProcessed = !is.null(lastZipFile) &&
    !is.null(lastZipDate) &&
    lastZipFile == zipFile && lastZipDate >= mtime
  if (alreadyProcessed)
    return (
      list(
        n = 0,type = "ok",
        msg = paste0("Zip-file ",zipFile, " already processed"),
        lastZipFile = lastZipFile,lastZipDate = as.character(lastZipDate)
      )
    )
  files = unzip(zipFile,list = TRUE)
  # remove directories and zero length files
  files = files[files$Length > 0,][,-2] # Remove length after cleanup
  if (is.null(files) || length(files) == 0)
    return(list(
      n = 0, type = "error", msg = paste0("No files found in ",zipFile)
    ))
  files = files[str_detect(files$Name,inZipPath),]
  if (length(files) == 0)
    return (list(
      n = 0,type = "error",msg = paste0("No files with inZipPath ", inZipPath,
                                        " found in ",zipFile)
    ))
  files$baseName = basename(files$Name)
  # Get  files in destination path
  inDestination =
    na.omit(file.info(file.path(destinationPath,files$baseName))[,"mtime",drop =
                                                                   FALSE])
  inDestination$baseName = basename(rownames(inDestination))
  files = join(files,inDestination,by = "baseName")
  files$sameDate =  !(is.na(files$mtime))  &
    abs(as.POSIXct(as.character(files$mtime)) -
          as.POSIXct(as.character(files$Date))) < 60
  sameDateFiles = sum(files$sameDate)
  if (sameDateFiles == nrow(files))
    return (
      list(
        n = 0,type = "ok",
        msg = paste0("All files in ",basename(zipFile), " already extracted."),
        lastZipFile = zipFile,lastZipDate = as.character(mtime)
      )
    )
  toExtract = files[!files$sameDate,"Name"]
  unz = try(unzip(
    zipFile,toExtract,overwrite = TRUE,junkpaths = TRUE,exdir = destinationPath,
    setTimes = TRUE
  ),
  silent = TRUE)
  if (inherits(unz, "try-error"))
    return (list(
      n = 0,type = "error",msg =  attr(unz,"condition")$message
    ))
  extractedFiles = length(unz)
  list(
    n = extractedFiles,type = "ok", msg = "New files extracted",
    files = basename(unz),
    lastZipFile = zipFile,lastZipDate = as.character(mtime)
  )
}