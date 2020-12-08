#'@title readParameters
#'@description Reads the 'parameters.csv' file. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `betavalues` data.frame of model parameters from parameters.csv



readParameters <- function(file.output.list,if_estimate,if_estimate_simulation,
                           batch_mode) {
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  path<-path_results
  
  
  #define column classes and names
  filebetas <- paste0(path,run_id,"_parameters.csv")
  Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric")
  NAMES<- c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmCorrGroup")   
  
  #read file  
  betavalues<-importCSVcontrol(filebetas,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  betavalues$sparrowNames<-trimws(betavalues$sparrowNames,which="both")
  #make fixed and required names lowercase
  betavalues$sparrowNames<-ifelse(tolower(betavalues$sparrowNames) %in% as.character(getVarList()$varList),tolower(betavalues$sparrowNames),betavalues$sparrowNames)
  
  
  
  #replace NAs with 0 in numeric columns
  for (c in names(betavalues)){
    test<-eval(parse(text=paste0("betavalues$",c)))
    if (class(test)=="numeric"){
      test<-ifelse(is.na(test),0,test)
      eval(parse(text=paste0("betavalues$",c,"<-test")))
    }
  }
  
  
  #create parmConstant
  betavalues$parmConstant<-ifelse(betavalues$parmInit==betavalues$parmMax & betavalues$parmInit==betavalues$parmMin & betavalues$parmInit!=0,1,0)
  betavalues<-as.data.frame(betavalues)
  betavalues<-betavalues[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmConstant","parmCorrGroup"),names(betavalues))]
  
  #test for "SOURCE" in parmType column
  sources<-betavalues[which(betavalues$parmType=="SOURCE"),]
  if (nrow(sources)==0){
    
    message("NO SOURCES FOUND IN PARAMETERS FILE.\nRUN EXECUTION TERMINATED.")
    if (batch_mode=="yes"){#if batch output message to log
      cat("NO SOURCES FOUND IN PARAMETERS FILE.\nRUN EXECUTION TERMINATED.")
    }
    
    errorOccurred("readParameters.R",batch_mode)
  }
  
  #test for no parameters (parmMax==0)
  testMax<-betavalues[which(betavalues$parmMax!=0),]
  if (nrow(testMax)==0 & (if_estimate=="yes" |if_estimate_simulation=="yes")){
    
    message("NO PARAMETERS FOUND FOR ESTIMATION IN PARAMETERS FILE.\nALL PARAMETERS FOUND HAVE parmMAX==0\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    if (batch_mode=="yes"){#if batch output message to log
      cat("NO PARAMETERS FOUND FOR ESTIMATION IN PARAMETERS FILE.\nALL PARAMETERS FOUND HAVE parmMAX==0\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
    }
    
    errorOccurred("readParameters.R",batch_mode)
  }
  
  #test for missing values
  missing<-character(0)
  for (c in 4:length(betavalues)){
    testNA<-betavalues[c]
    testNA<-which(is.na(testNA))
    if (length(testNA)!=0){
      missing<-c(missing,names(betavalues)[c])
    }
  }
  if (length(missing)!=0){
    
    message(paste0(" \nMISSING VALUES FOUND IN THE FOLLOWING COLUMNS OF THE PARAMETERS FILE:\n \n",paste(missing,collapse="\n"),"\n \nRUN EXECUTION TERMINATED."))
    if (batch_mode=="yes"){#if batch output message to log
      cat(" \nMISSING VALUES FOUND IN THE FOLLOWING COLUMNS OF THE PARAMETERS FILE:\n \n",paste(missing,collapse="\n"),"\n \nRUN EXECUTION TERMINATED.",sep="")
    }
    
    
    errorOccurred("readParameters.R",batch_mode) 
  }
  
  
  
  
  return(betavalues)
  
}#end function


