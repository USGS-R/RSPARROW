#'@title findControlFiles
#'@description checks to make sure all necessary control files are present in the results 
#'            directory \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: errorOccurred.R \\cr
#'@param path_user character string path to RSPARROW user directory containing results, data, 
#'       and gis subdirectories
#'@param if_userModifyData yes/no indicating whether or not the userModifyData.R control file 
#'       is to be applied
#'@param create_initial_dataDictionary yes/no control setting indicating whether a 
#'       dataDictionary.csv control file will be created based on the variables found in the data1 file
#'@param create_initial_parameterControlFiles yes/no indicating if new parameter files should 
#'       be generated based on the dataDictionary.csv control file



findControlFiles<-function(path_user,if_userModifyData,
                           create_initial_dataDictionary, create_initial_parameterControlFiles){
  
  
  path_results <- paste0(path_user,.Platform$file.sep,results_directoryName) # location of the results directory
  
  
  #save control files
  filesList<-c("sparrow_control.R")
  if (if_userModifyData=="yes"){
    filesList<-c(filesList,"userModifyData.R")
  }
  if (create_initial_dataDictionary=="no"){
    filesList<-c(filesList,"dataDictionary.csv")
  }
  if (create_initial_parameterControlFiles=="no"){
    filesList<-c(filesList,"design_matrix.csv","parameters.csv")
  }
  
  fileExist<-sapply(filesList, function(x) file.exists(paste0(path_results,.Platform$file.sep,x)))
  
  fileExist<-data.frame(success = t(fileExist)[1,])
  fileExist$path<-paste0(path_results,.Platform$file.sep,filesList)
  fileExist<-fileExist[(!fileExist$success),]
  
  if (nrow(fileExist)!=0){
    for (x in fileExist$path){
      message(paste0("MISSING CONTROL FILES.\n \n",x,"\n \nRUN EXECUTION TERMINATED"))
    }
    errorOccurred("findControlFiles.R",batch_mode)
  }
  
  
}
