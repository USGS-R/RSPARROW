#'@title isScriptSaved
#'@description outputs question to the console asking if the current sparrow_control.R file 
#'            has been saved and stopping execution until the user indicates that it has been saved \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'@param scriptName name of functional routine or '.R' control file
#'@param testDir subdirectory in the results directory with the current run_id.  If this 
#'       directory exists it will be overwritten, if the user indicates 'yes' to the question in the console
#'@return `saved` 1 indicated that the sparrow_control.R file has been saved or 2 indicated 
#'            that it has not been saved and execution should be halted



isScriptSaved<-function(scriptName,testDir){
  
  msgText<-cat("Did you save the active control file \n",scriptName," \nand all '*.csv' control files?\n \n",sep="")
  
  if (dir.exists(testDir)){
    msgText<-cat(msgText,"Are ALL results files closed?\n \nPrevious model subdirectory exists with current run_id\nAll files in the estimate, predicts, maps, and scenarios directories will be deleted if option '1' is selected below\n \nTo overwrite results files ALL results files MUST be closed including _summary.txt and _diagnostic_plots.pdf.\n \n",sep="")
  }
  saved<-menu(c("Yes, I have saved all control files. Continue the current run.",
                "No, I haven't saved all control files.  Cancel current run."),title=msgText)
  saved<-ifelse(saved==1,TRUE,FALSE)
  return(saved)
}
