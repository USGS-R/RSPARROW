#'@title createInitialDataDictionary
#'@description Creates a new dataDictionary.csv file based on the column names of `data1.csv`, 
#'            adds missing, REQUIRED, and FIXED variables with `data1UserName=="NA"`, and opens the new 
#'            dataDictionary.csv file for the user to edit. RSPARROW execution will be terminated once the 
#'            dataDictionary.csv file is created. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item readData.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param input_data_fileName name of users data1 file
#'@param create_initial_parameterControlFiles yes/no indicating if new parameter files should 
#'       be generated based on the dataDictionary.csv control file



createInitialDataDictionary<-function(file.output.list,input_data_fileName,
                                      create_initial_parameterControlFiles){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  if (!file.exists(file.path(paste0(dirname(path_results),.Platform$file.sep,"dataDictionary.csv")))){
    #read data1 or indata
    data1<-readData(file.output.list,input_data_fileName)
    
    #create varnames file
    initialVarnames<-as.data.frame(matrix(rep(NA,length(data1)),ncol=4,nrow=length(data1)))
    names(initialVarnames)<-c("varType","sparrowNames","data1UserNames","varunits")
    initialVarnames$data1UserNames<-names(data1)
    
    #match any required or fixed varnames
    fixed<-as.character(getVarList()$fixNames)
    required<-as.character(getVarList()$reqNames)
    initialVarnames$sparrowNames = ifelse(tolower(initialVarnames$data1UserNames) %in% c(fixed,required),tolower(initialVarnames$data1UserNames),NA)
    initialVarnames$varType = ifelse(tolower(initialVarnames$data1UserNames) %in% fixed,"FIXED",
                                     ifelse(tolower(initialVarnames$data1UserNames) %in% required,"REQUIRED",NA))
    
    #get explanations for fixed and required variables
    initialVarnames<-merge(initialVarnames,getVarList()$explanation,by="sparrowNames",all.x=TRUE)
    
    #reorder
    initialVarnames<-initialVarnames[match(names(data1),initialVarnames$data1UserNames),match(c("varType","sparrowNames","data1UserNames","varunits","explanation"),names(initialVarnames))]
    
    missingRequired<-required[which(!required %in% initialVarnames$sparrowNames)]
    if (length(missingRequired)!=0){
      message("MISSING REQUIRED sparrowNames :")
      for (i in missingRequired){
        message(i)
      }
    }#if missing required
    cat("\n \n")
    missingFixed<-fixed[which(!fixed %in% initialVarnames$sparrowNames)]
    if (length(missingFixed)!=0){
      message("MISSING FIXED sparrowNames : ")
      for (i in missingFixed){
        message(i)
      }
    }#if missing fixed
    
    #add missing names to top of initialvarnames
    missing<-data.frame(sparrowNames = c(missingRequired,missingFixed))
    if (nrow(missing)!=0){
      missing<-merge(missing,getVarList()$explanation, by="sparrowNames")
      missing$varType<-ifelse(missing$sparrowNames %in% fixed,"FIXED","REQUIRED")
      missing$data1UserNames<-rep(NA,nrow(missing))
      missing$varunits<-missing$data1UserNames
      missing<-missing[,match(c("varType","sparrowNames","data1UserNames","varunits","explanation"),names(missing))]
      initialVarnames<-rbind(missing,initialVarnames)
    }
    
    #write varnames
    fwrite(file=paste0(dirname(path_results),.Platform$file.sep,"dataDictionary.csv"),initialVarnames,
           row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec=csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
    cat("\n \n")
    message(paste0("INITIAL dataDictionary FILE : ",paste0(dirname(path_results),.Platform$file.sep,"dataDictionary.csv")," AVAILABLE FOR EDIT"))
    shell.exec(paste0(dirname(path_results),.Platform$file.sep,"dataDictionary.csv"))
    
    if (create_initial_parameterControlFiles=="no"){
      cat("\n \n")
      message("RUN EXECUTION TERMINATED")
    }
  }else{#varnames already exists
    message(paste0(paste0(dirname(path_results),.Platform$file.sep,run_id,"_dataDictionary.csv")," ALREADY EXISTS.\n
NEW dataDictionary FILE NOT CREATED.\n
SET create_initial_dataDictionary<-'no' to RUN RSPARROW WITH CURRENT dataDictionary."))
    if (create_initial_parameterControlFiles=="no"){
      cat("\n \n")
      message("RUN EXECUTION TERMINATED")
    }
  }
  
}#end function

