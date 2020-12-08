#'@title makePaths
#'@description creates all internal path variables for file input/output \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item named.list.R\} \\cr
#'@param path_user character string path to RSPARROW user directory containing results, data, 
#'       and gis subdirectories
#'@param path_master character string path to RSPARROW_master directory.  Internally reset to 
#'       'RSPARROW_master/R/' subdirectory
#'@param run_id character string control setting indicating the current model name
#'@param results_directoryName character string indicating the users results subdirectory name
#'@param data_directoryName character string control setting indicating the name of the data 
#'       directory
#'@param gis_directoryName character string control setting indicating the name of the gis 
#'       directory



makePaths<-function(path_user, path_master,run_id,results_directoryName,data_directoryName,gis_directoryName, envir = .GlobalEnv){
  
  path_data <- paste0(path_user,.Platform$file.sep,data_directoryName,.Platform$file.sep)       # location of the DATA1 file
  path_results <- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep) # location of the results directory
  path_gis <- paste0(path_user,.Platform$file.sep,gis_directoryName)          # GIS shape files (necessary to exclude the slash at end)
  
  #test if data, results, and gis are all at same level
  badPath<-NA
  for (p in c("path_data","path_results","path_gis")){
    path<-get(p)
    if (p=="path_results"){
      path<-paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep)
    }
    if (!dir.exists(path)){
      badPath<-c(badPath,p)
    }
  }
  if (length(na.omit(badPath))!=0){
    
    message(paste0("ERROR : THE FOLLOWING REQUIRED PATHS ARE NOT FOUND IN THE USER DIRECTORY\n ",
                  paste0(" \n",na.omit(badPath)," : ",get(na.omit(badPath)),"\n"),"\nPATHS TO DATA, GIS, AND RESULTS MUST ALL EXIST IN THE USER DIRECTORY : \n ",
                  path_user," \nTHE CONTROL FILE SHOULD BE RUN FROM THE UPPER LEVEL OF THE RESULTS DIRECTORY SHOWN HERE : \n ",
                  paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep),"\nRUN EXECUTION TERMINATED"))
    errorOccurred("makePaths.R",batch_mode)
  }
  path_main<-path_master
  path_src <- paste0(path_main,.Platform$file.sep,"src",.Platform$file.sep)
  path_master <- paste0(path_main,.Platform$file.sep,"R",.Platform$file.sep)
  
  for (n in ls()[which(!ls() %in% c("p","badPath","envir","path"))]){
    assign(n,get(n),envir=.GlobalEnv)
  }

}
