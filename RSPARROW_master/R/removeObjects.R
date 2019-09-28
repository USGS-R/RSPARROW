#'@title removeObjects
#'@description Removes objects from the global environment if the object exists. Warnings are 
#'            suppressed. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr



removeObjects<-function(remove.list){
  suppressWarnings(rm( list = Filter( exists, remove.list),envir = .GlobalEnv ))
}

