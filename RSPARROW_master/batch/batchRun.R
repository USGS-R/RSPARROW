#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchRun.R","batch.RData",res))
  
  if (RSPARROW_errorOption=="yes"){
    #errorhandle
    backupOptions<-list(error = options()$error,
                        show.error.locations = options()$show.error.locations,
                        keep.source = options()$keep.source)
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
  }#end Error
  
  
  if (exists("runScript")){
    remove(list=c("runScript","runRsparrow"))
    
    
    devtools::load_all(path_main,recompile = FALSE)
    
    unPackList(lists = list(file.output.list = file.output.list,
                            class.input.list = class.input.list,
                            min.sites.list = min.sites.list,
                            scenario.input.list = scenario.input.list,
                            estimate.input.list = estimate.input.list,
                            mapping.input.list = mapping.input.list),
               parentObj = list(NA,
                                NA,
                                NA,
                                NA,
                                NA,
                                NA))
    
    runScript<-"yes"
    runRsparrow<-"yes"
    sink(file=paste0(path_results,.Platform$file.sep,"batchSessionInfo",.Platform$file.sep,run_id,"_log.txt"),split=TRUE)
    cat("\n \n")
    cat("RSPARROW MODEL NAME: ",run_id,sep="")
    cat("\n \n")
    if (select_scenarioReachAreas=="yes"){
      cat("SCENARIO NAME: ",scenario_name,sep="")
      cat("\n \n")
    }
    cat("OUTPUT DIRECTORY: ",path_results,sep="")
    cat("\n \n")
    
    # Section 2. DATA1 input and data preparation
    
    if (load_previousDataImport=="yes"){
      fileName<-strsplit(path_results,.Platform$file.sep)[[1]]
      fileName<-paste(fileName[1:length(fileName)-1],collapse = .Platform$file.sep)
      fileName<-paste0(fileName,.Platform$file.sep,gsub(".csv","",input_data_fileName),"_priorImport")
      #check if file exists
      if (file.exists(fileName)){
        load(file=fileName)  
      }else{
        cat("ERROR : ",fileName," NOT FOUND\n SET load_previousDataImport<-'no'.\n RUN EXECUTION TERMINATED.",sep="")
        errorOccurred("batchRun.R",batch_mode)
      }
      
      
    }else{
      dataInputPrep(#for readData
        file.output.list,input_data_fileName,
        #for checkData1NavigationVars
        if_reverse_hydseq,
        #for createVerifyNavigationVars
        if_verify_demtarea,calculate_reach_attribute_list,
        mapping.input.list,
        #for all
        batch_mode)
    }
    
    ###############################################################
    #runRsparrow

    startModelRun(file.output.list,
                  if_estimate,if_estimate_simulation,
                  if_boot_estimate,if_boot_predict,enable_ShinyApp,
                  #createSubdataSorted
                  filter_data1_conditions,data1,
                  #applyUserModify
                  if_userModifyData,
                  data_names,
                  #checkClassificationVars
                  class.input.list,
                  #selectCalibrationSites
                  min.sites.list,
                  #selectValidationSites
                  if_validate,iseed,pvalidate,
                  #findMinMaxLatLon
                  mapping.input.list,
                  #controlFileTasksModel
                  estimate.input.list,
                  if_predict,biters,
                  scenario.input.list,
                  #modelCompare
                  compare_models,modelComparison_name,if_spatialAutoCorr,
                  #shinyMap2
                  add_vars,
                  batch_mode,
                  RSPARROW_errorOption)
    
    
    save.image(file=paste0(path_results,.Platform$file.sep,"batchSessionInfo",.Platform$file.sep,run_id,".RData"))

    
    sink()
  }
  if (RSPARROW_errorOption=="yes"){
    options(backupOptions)  
  }
}