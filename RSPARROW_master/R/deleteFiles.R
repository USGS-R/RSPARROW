#'@title deleteFiles
#'@description Deletes all files from estimate, predict, maps, and scenarios subdirectories.  
#'            Subroutine is only called when model estimation is requested via the control settings 
#'            if_estimate<-"yes" or if_estimate_simulation<-"yes". \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'@param path_results path to results model subdirectory



deleteFiles<-function(path_results){
  
  
  #delete files from prediction, mapping, and scenario folders
  folders<-paste0(path_results,c("estimate","predict","maps","scenarios"),.Platform$file.sep)
  for (f in folders){
    filesList<-list.files(f,recursive = TRUE,full.names=TRUE)
    if (length(filesList)!=0){
      msgText<-paste0("Deleting files from '",basename(f),"' subdirectory...")
      message(msgText)
      unlink(filesList,recursive = TRUE)
    }
  }
  
}#end function
