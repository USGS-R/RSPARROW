#'@title syncVarNames
#'@description Updates the 'dataDictionary.csv' varType column with the user designated 
#'            parmTypes from the 'parameters.csv' file. \\cr \\cr
#'Executed By: createDirs.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



syncVarNames<-function(file.output.list,batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  path_results<-dirname(path_results)
  
  #read dataDictionary
  #set column types and names  
  Ctype <- c("character","character","character","character","character")
  NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
  
  #file to import
  filein<-paste0(path_results,.Platform$file.sep,"dataDictionary.csv")
  
  #check file for correct number of fields
  #import dataDictionary
  data_names<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
  data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")  
  #make fixed and required names lowercase
  data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)
  
  #remove rows with blank sparrowNames
  data_names<-data_names[which(!is.na(data_names$sparrowNames) & data_names$sparrowNames!=""),]
  #remove exact duplicates
  data_names<-data_names[!duplicated(data_names),]
  
  ###################
  #read parameters
  #set column types and names  
  Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric")
  NAMES<- c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmCorrGroup")   
  
  #file to import
  filein<-paste0(path_results,.Platform$file.sep,"parameters.csv")
  
  #check file for correct number of fields
  #import parameters
  betavalues<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  betavalues$sparrowNames<-trimws(betavalues$sparrowNames,which="both")
  #make fixed and required names lowercase
  betavalues$sparrowNames<-ifelse(tolower(betavalues$sparrowNames) %in% as.character(getVarList()$varList),tolower(betavalues$sparrowNames),betavalues$sparrowNames)
  
  #compare varType and parmType
  testVarType<-merge(data_names,betavalues[,c(1,7)], by="sparrowNames")
  testVarType<-testVarType[which(testVarType$varType!=testVarType$parmType),]
  testVarType<-testVarType[,c(2,1,3:6)]
  
  if (nrow(testVarType)!=0){
    message("THE FOLLOWING ROWS OF THE dataDictionary.csv FILE HAVE BEEN UPDATED \nWITH varTypes MATCHING THE parmTypes IN THE parameters.csv FILE")
    print(testVarType)
    
    NAMES<-names(data_names)
    data_names$rowOrder<-as.numeric(rownames(data_names))
    data_names<-merge(data_names,betavalues[,c(1,7)],by="sparrowNames",all.x=TRUE)
    data_names$varType<-ifelse(!is.na(data_names$parmType),data_names$parmType,data_names$varType)
    data_names<-data_names[order(data_names$rowOrder),]
    data_names<-data_names[,1:5]
    data_names<-data_names[,c(2,1,3:5)]
    fwrite(file=paste0(path_results,.Platform$file.sep,"dataDictionary.csv"),data_names,
           row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec=csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  }#end testVartype
  
  
  
}#end function
