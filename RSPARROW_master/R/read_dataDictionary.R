#'@title read_dataDictionary
#'@description Reads the 'dataDictionary.csv' file. \\cr \\cr
#'Executed By: dataInputPrep.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `data_names` data.frame of variable metadata from data_Dictionary.csv file

read_dataDictionary <- function(file.output.list,batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  path<-path_results
  
  
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  
  filein <- paste0(path,run_id,"_dataDictionary.csv")
  Ctype <- c("character","character","character","character","character")
  NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
  
  #check file for correct number of fields
  #import dataDictionary
  data_names<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  
  #trim whitespaces
  data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
  data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")  
  #make fixed and required names lowercase
  data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)
  
  blankSparrow<-data_names[which(is.na(data_names$sparrowNames)|data_names$sparrowNames==""),]
  if (nrow(blankSparrow)!=0){
    message(" \nsparrowName is BLANK in data dictionary at row(s) : ", paste(rownames(blankSparrow),collapse=", "),".  These rows have been removed.")
    cat("\n \n")
    if (batch_mode=="yes"){
      cat(" \nsparrowName is BLANK in data dictionary at row(s) : ", paste(rownames(blankSparrow),collapse=", "),".  These rows have been removed.")
      cat("\n \n")     
    }
  }
  
  data_names<-data_names[which(!is.na(data_names$sparrowNames) & data_names$sparrowNames!=""),]
  #remove exact duplicates
  data_names<-data_names[!duplicated(data_names),]
  
  #test if add_vars in data_names
  if(!is.na(add_vars)){
    if (any(!add_vars %in% data_names$sparrowNames)){
      message(paste0("WARNING: add_vars MISSING FROM dataDictionary sparrowNames : ",paste(add_vars[which(!add_vars %in% data_names$sparrowNames)],collapse=","),"\n \n"))
      cat("\n \n")   
      if (batch_mode=="yes"){
        cat("WARNING: add_vars MISSING FROM dataDictionary sparrowNames : ",paste(add_vars[which(!add_vars %in% data_names$sparrowNames)],collapse=","),"\n \n")
        cat("\n \n")     
      }
      add_vars<-add_vars[which(add_vars %in% data_names$sparrowNames)]
    }
  }     
  
  
  return(data_names)
  
}#end function
