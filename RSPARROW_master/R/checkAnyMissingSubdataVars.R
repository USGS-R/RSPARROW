#'@title checkAnyMissingSubdataVars
#'@description Identifies REQUIRED system variables and parameter variables with any missing 
#'            or zero values and prints a warning in the console \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item checkingMissingVars.R
#'             \\item errorOccurred.R
#'             \\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param subdata data.frame input data (subdata)
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkAnyMissingSubdataVars <- function(subdata,betavalues,batch_mode) {
  
  #get missing values
  missing<-checkingMissingVars(subdata, data_names = NA, betavalues,  
                               types = c("datalstCheck","xlnames") , allMissing = FALSE,
                               returnData = FALSE)
  unPackList(lists = list(missing = missing),
             parentObj = list(NA))
  
  
  if(k>0) { 
    reqMissingSubdataVariable<-datalstMissingdata[1:k]
    reqMissingSubdataVariable<-reqMissingSubdataVariable[which(reqMissingSubdataVariable %in% as.character(getVarList()$reqNames))]
    if (length(reqMissingSubdataVariable)!=0){
      for (i in reqMissingSubdataVariable){
        message(paste0(" \nWARNING: THIS REQUIRED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n "))
        if (batch_mode=="yes"){
          cat(paste0(" \nWARNING: THIS REQUIRED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n "))
          if (i==reqMissingSubdataVariable[length(reqMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
      }
    }else
      reqMissingSubdataVariableMessage<-""
    
    fixMissingSubdataVariable<-datalstMissingdata[1:k]
    fixMissingSubdataVariable<-fixMissingSubdataVariable[which(fixMissingSubdataVariable %in% as.character(getVarList()$fixNames))]
    if (length(fixMissingSubdataVariable)!=0){
      for (i in fixMissingSubdataVariable){
        message(paste0(" \nWARNING: THIS FIXED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n "))
        if (batch_mode=="yes"){
          cat(paste0(" \nWARNING: THIS FIXED VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\n "))
          if (i==fixMissingSubdataVariable[length(fixMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
      }
    }else
      fixMissingSubdataVariableMessage<-""
    
    paramMissingSubdataVariable<-datalstMissingdata[1:k]
    paramMissingSubdataVariable<-paramMissingSubdataVariable[which(paramMissingSubdataVariable %in% as.character(xlnames))]
    if (length(paramMissingSubdataVariable)!=0){
      for (i in paramMissingSubdataVariable){
        message(paste0(" \nERROR: THIS PARAMETER VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\nRUN EXECUTION TERMINATED"))
        if (batch_mode=="yes"){
          cat(paste0(" \nERROR: THIS PARAMETER VARIABLE HAS SELECTED MISSING VALUES IN SUBDATA:",i,"\nRUN EXECUTION TERMINATED"))
          if (i==paramMissingSubdataVariable[length(paramMissingSubdataVariable)]){
            cat("\n \n")
          }
        }
        errorOccurred("checkAnyMissingSubdataVars.R",batch_mode)
      }
    }else
      paramMissingSubdataMessage<-""
    
  }
  
  
  
  
}#end function




