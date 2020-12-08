#'@title copyPriorModelFiles
#'@description Copies the previously executed model control files into the top level user's 
#'            results directory. RSPARROW is terminated after files are copied and the copied control script, 
#'            sparrow_control.R, is opened in Rstudio. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: errorOccurred.R \\cr
#'@param activeFile character string path to sparrow_control.R file at currently top level of 
#'       user's results directory
#'@param old_run_id character string indicating the archived model subdirectory containing 
#'       control files to be copied
#'@param path_master character string path to RSPARROW_master directory.  Internally reset to 
#'       'RSPARROW_master/R/' subdirectory
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



copyPriorModelFiles<-function(activeFile,old_run_id, path_master,batch_mode){
  
  closed<-menu(c("Yes","No"),title=cat("You have selected to run copy_PriorModelFiles <-'",old_run_id,"'\n \nPlease close all active control files now.  Including \n",activeFile," \nSelect 'Yes' after all files are closed.\n \nTo cancel copy_PriorModelFiles select 'No'.",sep=""))
  if (closed==1){
    if (basename(activeFile)!="sparrow_control.R"){
      message("copy_PriorModelFiles MUST be run from the sparrow_control.R file in the results directory.\n copy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED.")
      
    }else{#copy files
      #get path_results
      path_results<-dirname(activeFile)
      path_old<-paste0(path_results,.Platform$file.sep,old_run_id,.Platform$file.sep)
      path_oldFile<-paste0(path_old,old_run_id,"_sparrow_control.R")
      
      #replace path_master in old file with activeFile path_master
      #read control file as text
      x <- readLines(path_oldFile)
      #find where path_master is designated
      editthis<-x[which(regexpr("path_master<-",gsub(" ","",x))>0 &  regexpr("#path_master",x)<0)]
      #replace with current path_master
      y <- gsub( editthis, paste0("path_master<-'",path_master,"'"), x )
      #overwrite the file
      cat(y, file=path_oldFile, sep="\n")
      
      #list necessary files
      filesList<-c("sparrow_control.R",
                   "parameters.csv",
                   "design_matrix.csv",
                   "userModifyData.R",
                   "dataDictionary.csv")
      filesListFrom<-paste0(old_run_id,"_",filesList)
      
      #copy files
      for (f in 1:length(filesList)){
        if (file.exists(paste0(path_old,filesListFrom[f]))){
          file.copy(from = paste0(path_old,filesListFrom[f]),
                    to=paste0(path_results,.Platform$file.sep,filesList[f]),
                    overwrite = TRUE)
        }else{
          message("MISSING control file \n",paste0(path_old,filesListFrom[f]),"\ncopy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED. ")
          errorOccurred("copyPriorModelFiles.R",batch_mode)
        }
      }
      
      #no error
      file.edit(paste0(path_results,.Platform$file.sep,"sparrow_control.R"))
      message(paste0("copy_PriorModelFiles COMPLETE.  Control files ready for edit in \n",path_results))
      
      
    }
    
  }else{
    message("Files NOT closed.  copy_PriorModelFiles FAILED.\n  RUN EXECUTION TERMINATED.")
  }#make sure closed
  
  #assign("runOld","yes",.GlobalEnv)
  
  
}#end function
