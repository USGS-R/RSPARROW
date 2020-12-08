#'@title checkMissingData1Vars
#'@description Identifies REQUIRED system variables with all missing/zero values in the 
#'            `data1.csv` file and adds missing variables to the file with all NA values. Outputs a printed warning 
#'            for users and a list of missing variables as a message in console. \\cr \\cr
#'Executed By: dataInputPrep.R \\cr
#'Executes Routines: \\itemize\{\\item checkingMissingVars.R
#'             \\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param data1 input data (data1)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `data1`  data1 input data.frame with missing variables added



checkMissingData1Vars <- function(data1,batch_mode) {
  
  #get missing values
  missing<-checkingMissingVars(data1, data_names=NA, betavalues = NA,  
                               types = c("datalstCheck") , allMissing = TRUE,
                               returnData = TRUE)
  unPackList(lists = list(missing = missing),
             parentObj = list(NA))
  
  MissingData1VariableMessage <- ""
  reqMissingData1VariableMessage <- ""
  fixMissingData1VariableMessage <- ""
  
  if(k>0) { 
    reqMissingData1Variable<-datalstMissingdata[1:k]
    reqMissingData1Variable<-reqMissingData1Variable[which(reqMissingData1Variable %in% as.character(getVarList()$reqNames))]
    if (length(reqMissingData1Variable)!=0){
      for (i in reqMissingData1Variable){
        reqMissingData1VariableMessage <- paste0(" \nWARNING: THIS REQUIRED VARIABLE HAS ALL MISSING VALUES IN DATA1:",i,"\n ")
      }
    }else
      reqMissingData1VariableMessage<-""
    
    fixMissingData1Variable<-datalstMissingdata[1:k]
    fixMissingData1Variable<-fixMissingData1Variable[which(fixMissingData1Variable %in% as.character(getVarList()$fixNames))]
    if (length(fixMissingData1Variable)!=0){
      for (i in fixMissingData1Variable){
        fixMissingData1VariableMessage <- paste0(" \nWARNING: THIS FIXED VARIABLE HAS ALL MISSING VALUES IN DATA1:",i,"\n ")
      }
    }else
      fixMissingData1VariableMessage<-""
  }
  
  #output messages
  if (reqMissingData1VariableMessage!=""){
    cat("\n\n")
    message(reqMissingData1VariableMessage)
    cat("\n\n")
    if (batch_mode=="yes"){
      cat(reqMissingData1VariableMessage)
      cat("\n\n")  
    }
  }
  if (fixMissingData1VariableMessage!=""){
    cat("\n\n")
    message(fixMissingData1VariableMessage)
    cat("\n\n")
    if (batch_mode=="yes"){
      cat(fixMissingData1VariableMessage)
      cat("\n\n")  
    }
  }
  
  
  
  return(data1)
  
}#end function
