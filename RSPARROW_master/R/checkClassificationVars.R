#'@title checkClassificationVars
#'@description Checks for missing or zero values in classification variables in section 5 of 
#'            the control script. If missing and/or zeros are found, a critical error has been found and 
#'            program execution will terminate. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item unPackList.R\} \\cr
#'@param subdata data.frame input data (subdata)
#'@param class.input.list list of control settings related to classification variables
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkClassificationVars<-function(subdata,class.input.list,batch_mode){
  
  
  unPackList(lists = list(class.input.list = class.input.list),
             parentObj = list(NA)) 
  
  if (!is.na(classvar)){
    #test for class var in names subdata
    testClassvar<-classvar[which(!classvar %in% names(subdata))]
    if (length(testClassvar)!=0){
      message(paste0("INVALID classvar ",paste(testClassvar,collapse=", ")," NOT FOUND IN dataDictionary.csv \nRUN EXECUTION TERMINATED"))
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID classvar ",paste(testClassvar,collapse=", ")," NOT FOUND IN dataDictionary.csv \nRUN EXECUTION TERMINATED",sep="")
      }
      errorOccurred("checkClassificationVars.R",batch_mode)
    }
  }
  
  #check that no NAs exist in the user definced classvar and class_landuse variables 
  colsOrder<-c(na.omit(c(classvar,class_landuse)))
  if (length(colsOrder)!=0){
    cols<-subdata[,which(names(subdata) %in% colsOrder)]
    cols<-cols[,match(colsOrder,names(cols))]
    for (c in 1:length(cols)){
      check<-cols[[c]]
      check<-check[which(is.na(check))]
      if (length(check)!=0){
        if (names(cols)[c] %in% classvar){
          type<-"classvar"
        }else{
          type<-"class_landuse"
        }
        cat("\n \n")
        message(paste("ERROR the following ",type," variable has MISSING values : ",names(cols)[c]))
        message(paste("\nMISSING VALUES IN 'classvar' AND/OR 'class_landuse' VARIABLES WILL CAUSE PROGRAM FAILURE!"))
        cat("\n \n")
        if (batch_mode=="yes"){
          cat("\n \n")
          cat(paste("ERROR the following ",type," variable has MISSING values : ",names(cols)[c]))
          cat(paste("\nMISSING VALUES IN 'classvar' AND/OR 'class_landuse' VARIABLES WILL CAUSE PROGRAM FAILURE!"))
          cat("\n \n") 
        }
        errorOccurred("checkClassificationVars.R",batch_mode)
      }
    }
  }
  
}#end function
