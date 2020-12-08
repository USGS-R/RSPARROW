#'@title removeObjects
#'@description Removes objects from the global environment if the object exists. Warnings are 
#'            suppressed. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'@param remove.list character vector of objects to remove



removeObjects<-function(remove.list){
  suppressWarnings(rm( list = Filter( exists, remove.list),envir = .GlobalEnv ))
}

