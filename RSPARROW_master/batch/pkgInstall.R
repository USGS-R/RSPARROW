#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("pkgInstall.R","batch.RData",res))
  
  
  
  install.packages(package, repos="http://ftp.ussg.iu.edu/CRAN/",quiet=TRUE)
  
  
}