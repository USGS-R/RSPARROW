#'@title createMasterDataDictionary
#'@description Compiles all archived dataDictionary.csv files in the user's results directory 
#'            by `run_id` from the earliest date forward. The master_dataDictionary.csv file is saved to the 
#'            upper level of the user's results directory. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



createMasterDataDictionary<-function(file.output.list,batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  #get all dataDictionary files in results directory with creation dates
  files<-list.files(dirname(path_results),full.names = TRUE, recursive = TRUE, pattern = "dataDictionary.csv") 
  dates<-file.info(files)$ctime
  filedates<-cbind(files,as.data.frame(dates))
  
  #format file name to extract run_ids
  filedates$run_id<-basename(as.character(filedates$files))
  filedates<-filedates[which(!filedates$run_id %in% c("dataDictionary.csv","master_dataDictionary.csv")),]                            
  filedates$run_id<-gsub("_dataDictionary.csv","",filedates$run_id)
  
  
  #order by date created
  filedates<-filedates[order(filedates$dates),]
  
  message("creating master_dataDictionary.csv")
  
  for (f in 1:nrow(filedates)){
    readFile<-TRUE
    
    #set column types and names  
    Ctype <- c("character","character","character","character","character")
    NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
    
    #file to import
    filein<-as.character(filedates$files[f])
    
    #check file for correct number of fields
    #read file
    data_names<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \n',filein,' NOT ADDED TO master_dataDictionary.csv\n \n')",
                                 file.output.list,FALSE,batch_mode)
    
    if (data_names!="error"){
      
      #trim whitespaces
      data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
      data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")  
      #make fixed and required names lowercase
      data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)
      
      #remove rows with blank sparrowNames
      data_names<-data_names[which(!is.na(data_names$sparrowNames) & data_names$sparrowNames!=""),]
      #remove exact duplicates
      data_names<-data_names[!duplicated(data_names),]
      
      #add run_id
      data_names$run_id<-filedates$run_id[f]
      data_names$duplicate_sparrowName<-0
      if (!exists("master")){#create master
        master<-data_names
      }else{
        #combine master with data_names and remove duplicates
        testDups<-rbind(data_names,master)
        testDups<-testDups[,1:5]
        testDups<-testDups[!duplicated(testDups,fromLast = FALSE)&!duplicated(testDups,fromLast = TRUE),] 
        testDups$run_id<-filedates$run_id[f]
        testDups$duplicate_sparrowName<-0
        data_names<-rbind(data_names,testDups)
        data_names<-data_names[duplicated(data_names),]
        
        if (nrow(data_names)!=0){
          master<-rbind(master,data_names)
          
          #flag duplicate sparrowNames
          dupNames<-aggregate(master$sparrowNames, by=list(master$sparrowNames),FUN = length)
          dupNames<-dupNames[which(dupNames$x>1),]
          master$duplicate_sparrowName<-ifelse(as.character(master$sparrowName) %in% as.character(dupNames$Group.1),1,0)
          
        }#if new names
      }
    }# valid data_names file
    
  }#end for f
  fwrite(file=paste0(dirname(path_results),.Platform$file.sep,"master_dataDictionary.csv"),master,
         row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec=csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  
  
  
  
  
}#end function

