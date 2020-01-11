#'@title checkMissingSubdataVars
#'@description Identifies REQUIRED system variables and user selected parameters with all 
#'            missing/zero values in `subdata` and outputs a list of variables with all missing values as message 
#'            in console. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item checkingMissingVars.R
#'             \\item errorOccurred.R
#'             \\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param subdata data.frame input data (subdata)
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkMissingSubdataVars <- function(subdata,betavalues,file.output.list,batch_mode) {
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  
  #get missing values
  missing<-checkingMissingVars(subdata, data_names, betavalues,  
                               types = c("datalstCheck","xlnames","vrnames") , 
                               allMissing = TRUE, returnData = FALSE)
  unPackList(lists = list(missing = missing),
             parentObj = list(NA))
  
  #output custom messages
  MissingSubdataVariableMessage <- ""
  if(k>0) {
    
    fixMissingSubdataVariable<-datalstMissingdata[1:k]
    for (i in unique(fixMissingSubdataVariable)){
      if(i %in% xlnames & i %in% datalstCheck & i %in% vrnames){
        problemFile<-paste("BOTH the ",path_results,run_id,"_parameters.csv and 
                         the ",path_results,run_id,"_dataDictionary.csv files",sep="")
      }else if (i %in% datalstCheck){
        problemFile<-"The required and fixed variables list"
      }else if (i %in% xlnames){
        problemFile<-paste("The ",path_results,run_id,"_parameters.csv file",sep="")
      }else{
        problemFile<-paste("The ",path_results,run_id,"_dataDictionary.csv file",sep="")
      }
      if (i %in% c(getVarList()$reqNames,xlnames)){
        msgText<-paste(" \nERROR: THIS REQUIRED VARIABLE FROM \n",problemFile,
                       " \nHAS ALL MISSING OR ZERO VALUES IN SUBDATA:",i,"\n \nRUN EXECUTION TERMINATED.\n ",sep="")
        message(msgText)
        errorOccurred("checkMissingSubdataVars.R",batch_mode)
      }else{
        msgText<-paste(" \nWARNING: THIS REQUIRED VARIABLE FROM \n",problemFile,
                       " \nHAS ALL MISSING OR ZERO VALUES IN SUBDATA:",i,"\n ",sep="")
      }
      message(msgText)
      
      if (batch_mode=="yes"){
        cat(msgText)
        if (i==fixMissingSubdataVariable[length(fixMissingSubdataVariable)]){
          cat("\n \n")
        }
      }
    }
  }
  
  
  
}#end function
