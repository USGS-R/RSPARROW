if (exists("runRsparrow")){
  if (runRsparrow=="yes"){   
    
    #load dynamic libraries
    dyn.load(paste0(path_master,"/src/deliv_fraction",.Platform$dynlib.ext))
    dyn.load(paste0(path_master,"/src/mptnoder",.Platform$dynlib.ext))
    dyn.load(paste0(path_master,"/src/ptnoder",.Platform$dynlib.ext))
    dyn.load(paste0(path_master,"/src/sites_incr",.Platform$dynlib.ext))
    dyn.load(paste0(path_master,"/src/tnoder",.Platform$dynlib.ext))
    dyn.load(paste0(path_master,"/src/sum_atts",.Platform$dynlib.ext))
    
    if (RSPARROW_errorOption=="yes"){
      
      
      #errorhandle
      backupOptions<-list(error = options()$error,
                          show.error.locations = options()$show.error.locations,
                          keep.source = options()$keep.source)
      #set custom RSPARROW options
      if (batch_mode=="no"){
        options(error=quote({
          cat('\nTraceback:\n');
          # Print full traceback of function calls. 
          #The '2' omits the outermost two function calls in the traceback.
          traceback(2);
          #print custom error message
          message("\nRSPARROW SYSTEM ERROR OCCURRED");
          #instruct the user to reset their options
          message('To reset user options in R use options(backupOptions)')}), 
          #show line numbers in traceback
          show.error.locations = TRUE,keep.source = TRUE)
      }else{#batch_mode=="yes"
        options(error = quote({
          #print custom message to console
          message("\nRSPARROW SYSTEM ERROR OCCURRED");
          #instruct the user to reset their options
          message('To reset user options in R use options(backupOptions)'); 
          #First dump error stack to file; not accessible by the R session.
          dump.frames("errorDump", to.file=TRUE, include.GlobalEnv=TRUE); 
          #sink to file
          sink(file=paste0(path_results,"error.log")); 
          #print custom error message to file
          cat("RSPARROW SYSTEM ERROR OCCURRED\n");
          #instruct the user to reset their options
          cat('To reset user options in R use options(backupOptions)\n \n'); 
          #Dump again to get error message and write it to error log; 
          #accessible by the R session.
          dump.frames(); 
          #Print simple error message to file
          cat(attr(last.dump,"error.message")); 
          cat('\nTraceback:');
          cat('\n'); #line space
          # Print full traceback of function calls. 
          #The '2' omits the outermost two function calls in the traceback.
          traceback(2); 
          shell.exec(paste0(path_results,"error.log"));
          sink() #end sink
        }),
        #show line numbers in traceback (shown as 'from #4')
        #line numbers count from the function call (i.e. `nestedFunc<-function(){` is line 1)
        show.error.locations = TRUE,keep.source = TRUE)
      }
    }    
    
    settingsEnv<-ls(envir = .GlobalEnv)[(ls(envir = .GlobalEnv) %in% 
                                           c(getCharSett(),getNumSett(),getOptionSett(),getShortSett(),getYesNoSett()))]
    
    executeRSPARROW(settingValues = lapply(settingsEnv, get),
                    
                    settingNames = settingsEnv,
                    activeFile,
                    
                    envir = .GlobalEnv)
    
    
    if (RSPARROW_errorOption=="yes"){
      options(backupOptions)  
    }
  }#end if
}#end exist rsparrow