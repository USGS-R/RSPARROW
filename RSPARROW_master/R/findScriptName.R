#'@title findScriptName
#'@description function to locate the current user directory \\cr \\cr
#'Executed By: sparrow_control.R\\cr
#'@return `path` character string path to current user directory

findScriptName <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  sysFrames<-sys.frames()[[1]]
  if (length(match) > 0) {
    # Rscript via command line
    path<-normalizePath(sub(needle, "", cmdArgs[match]))
  } else {
    ls_vars = ls(sysFrames)
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      path<-normalizePath(sysFrames$fileName) 
    } else if (!is.null(sysFrames$ofile)){
      # Source'd via R console
      path<-normalizePath(sysFrames$ofile)
    } else if (class(try({path<-normalizePath(rstudioapi::getActiveDocumentContext()$path)},TRUE))=="try-error"){
      message("Please select current control.R file.  Browser window may appear behind Rstudio.")
      path<-file.choose()
    }else        
      # RStudio Run Selection
      # http://stackoverflow.com/a/35842176/2292993 
      path<-normalizePath(rstudioapi::getActiveDocumentContext()$path)
  }
  
  
  assign("path_user",dirname(dirname(path)),envir = .GlobalEnv)
  return(path)
}
