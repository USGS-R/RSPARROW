#'@title createDirs
#'@description creates all model subdirectories for file output \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item syncVarNames.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param if_userModifyData yes/no indicating whether or not the userModifyData.R control file 
#'       is to be applied
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `complete`  TRUE/FALSE logical indicating whether or not function ran successfully



createDirs<-function(file.output.list,if_userModifyData,
                     batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  options(warn=-1)
  
  run_id<-basename(path_results)
  path_results<-dirname(path_results)
  
  #create main directory
  dir.create(paste0(path_results,.Platform$file.sep,run_id))
  #createsubdirectories
  dirList<-c("data",
             "estimate",
             "maps",
             "predict",
             "scenarios")
  
  sapply(dirList, function(x) dir.create(paste0(path_results,.Platform$file.sep,run_id,.Platform$file.sep,x)))
  if (batch_mode=="yes"){
    dir.create(paste0(path_results,.Platform$file.sep,run_id,.Platform$file.sep,"batchSessionInfo"))
  }
  
  #match varType in dataDictionary to parmTypes in parameters
  syncVarNames(file.output.list,batch_mode)
  
  #save control files
  filesList<-c("sparrow_control.R",
               "parameters.csv",
               "design_matrix.csv",
               "userModifyData.R",
               "dataDictionary.csv")
  if (if_userModifyData=="no"){
    filesList<-filesList[which(filesList!="userModifyData.R")]
  }
  fileCopy<-sapply(filesList, function(x) file.copy(paste0(path_results,.Platform$file.sep,x),
                                                    paste0(path_results,.Platform$file.sep,run_id,.Platform$file.sep,run_id,"_",x),overwrite=TRUE))
  fileCopy<-data.frame(success = t(fileCopy)[1,])
  fileCopy$path<-paste0(path_results,.Platform$file.sep,filesList)
  fileCopy<-fileCopy[which(!fileCopy$success),]
  
  if (nrow(fileCopy)!=0){
    for (x in fileCopy$path){
      message(cat("MISSING CONTROL FILES.\n \n",x,"\n \nRUN EXECUTION TERMINATED",sep=""))
    }
    complete<-FALSE
    errorOccurred("createDirs.R",batch_mode)
  }else{
    complete<-TRUE}
  
  
  return(complete)
  options(warn=0)
  
}#end function
