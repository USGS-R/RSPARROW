#'@title errorOccurred
#'@description Terminates program if an error occures in any routine. Error message is printed 
#'            to the console. \\cr \\cr
#'Executed By: \\itemize\{\\item batchRun.R
#'             \\item addVars.R
#'             \\item checkAnyMissingSubdataVars.R
#'             \\item checkClassificationVars.R
#'             \\item checkData1NavigationVars.R
#'             \\item checkMissingSubdataVars.R
#'             \\item copyPriorModelFiles.R
#'             \\item createDirs.R
#'             \\item createVerifyReachAttr.R
#'             \\item estimateNLLSmetrics.R
#'             \\item executeRSPARROW.R
#'             \\item findControlFiles.R
#'             \\item importCSVcontrol.R
#'             \\item makePaths.R
#'             \\item readParameters.R
#'             \\item selectParmValues.R
#'             \\item setNLLSWeights.R\} \\cr
#'@param scriptName name of functional routine or '.R' control file
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



errorOccurred<-function(scriptName,batch_mode){
  cat("\n \n")
  
  message(paste0("AN ERROR OCCURRED IN PROCESSING ", scriptName, "\n",
                "RUN EXECUTION TERMINATED."))
  if (batch_mode=="yes"){#if batch output message to log
    cat(" \nAN ERROR OCCURRED IN PROCESSING ", scriptName, "\n",
        "RUN EXECUTION TERMINATED.",sep="")
  }
  
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  exit() 
}
