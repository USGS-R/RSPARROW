#'@title executeRSPARROW
#'@description Runs manageDirVars functions and then proceeds with model excution either in 
#'            regular mode or batch mode \\cr \\cr
#'Executed By: runRsparrow.R \\cr
#'Executes Routines: \\itemize\{\\item batchRun.R
#'             \\item addVars.R
#'             \\item copyPriorModelFiles.R
#'             \\item createDirs.R
#'             \\item createInitialDataDictionary.R
#'             \\item createInitialParameterControls.R
#'             \\item dataInputPrep.R
#'             \\item deleteFiles.R
#'             \\item errorOccurred.R
#'             \\item findControlFiles.R
#'             \\item generateInputLists.R
#'             \\item getCharSett.R
#'             \\item getNumSett.R
#'             \\item getOptionSett.R
#'             \\item getShortSett.R
#'             \\item getYesNoSett.R
#'             \\item isScriptSaved.R
#'             \\item makePaths.R
#'             \\item named.list.R
#'             \\item openDesign.R
#'             \\item openParameters.R
#'             \\item openVarnames.R
#'             \\item outputSettings.R
#'             \\item removeObjects.R
#'             \\item setMapDefaults.R
#'             \\item setupMaps.R
#'             \\item startModelRun.R
#'             \\item testSettings.R
#'             \\item unPackList.R\} \\cr
#'@param settingValues user input values for all control settings
#'@param settingNames names of all control settings
#'@param activeFile character string path to sparrow_control.R file at currently top level of 
#'       user's results directory



executeRSPARROW<-function(settingValues,settingNames,activeFile, envir = .GlobalEnv){
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  settings<-settingValues
  
  names(settings)<- settingNames
  
  
  unPackList(lists = list(settings = settings),
             parentObj = list(NA))   
  
  #copy old model if requested
  if (!is.na(copy_PriorModelFiles)){
    copyPriorModelFiles(activeFile,copy_PriorModelFiles,path_master, batch_mode)
    runOld<-"yes"
  }else{
    runOld<-"no"
  }
  
  #trigger shiny only
  
  
  
  testDir<- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep) 
  
  findControlFiles(path_user,if_userModifyData,
                   create_initial_dataDictionary, create_initial_parameterControlFiles)
  
  
  if (runOld=="no"){
    
    #open control files for edit
    if (edit_Parameters=="yes"){openParameters(path_user,results_directoryName)}
    if (edit_DesignMatrix=="yes"){openDesign(path_user,results_directoryName)}
    if (edit_dataDictionary=="yes"){openVarnames(path_user,results_directoryName)}
    
    
    #Questions for user 
    {removeObjects(c("saved","runScript","run2","runOld",
                     "data1","GeoLines","lineShape","polyShape","data1_priorImport",
                     "subdata","BootBetaest","predict.list","BootUncertainties",
                     "sparrowEsts","DataMatrix.list","DataMatrixEstimate.list","HesResults","JacobResults"))
      if (activeFile==""){
        message("Please select current control.R file.  Browser window may appear behind Rstudio.")
        activeFile<-file.choose()
        assign("path_user",dirname(dirname(activeFile)),envir = .GlobalEnv)
      }
      saved<-isScriptSaved(activeFile,testDir)
      assign("saved",saved,envir = .GlobalEnv)
      if (!saved){
        cat("Please save active control file :\n",activeFile,"\nRun Execution Terminated.")
      }
      if (saved){
        #set path_main
        path_main<-path_master
        #set default values for any missing required mapping settings
        setMapDefaults(settings)
        
        ##test for invalid settings
        badSettings<-testSettings(settings,saved)
        if (nrow(badSettings)!=0){
          runScript<-"no"
          assign("runScript",runScript,envir = .GlobalEnv)
          cat("\n \n")
          print(badSettings)
          cat("\n \n")
          cat("Please fix all invalid settings.\nRun Execution Terminated.")
          cat("\n \n")
        }else{
          #make global paths
          makePaths(path_user,path_master,run_id,results_directoryName,data_directoryName,gis_directoryName)
          
          #rename control files
          runScript<-"yes"
          assign("runScript",runScript,envir = .GlobalEnv)
          #generate input lists
          updateSettings<-lapply(ls(envir = .GlobalEnv)[which(ls(envir = .GlobalEnv) %in% c(getCharSett(),
                                                                                            getNumSett(),
                                                                                            getOptionSett(),
                                                                                            getShortSett(),
                                                                                            getYesNoSett()))], get)
          names(updateSettings)<-ls(envir = .GlobalEnv)[which(ls(envir = .GlobalEnv) %in% 
                                                                c(getCharSett(),getNumSett(),getOptionSett(),getShortSett(),getYesNoSett()))]
          generateInputLists(updateSettings)
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
          
          #create initial varnames
          if (create_initial_dataDictionary=="yes"){
            createInitialDataDictionary(file.output.list,input_data_fileName,
                                        create_initial_parameterControlFiles)
            exit() 
          }
          #create initial design matrix and betas files
          if (create_initial_parameterControlFiles=="yes"){
            createInitialParameterControls(file.output.list,batch_mode)
            exit() 
          }
          
          
          if (runScript=="yes"){
            
            #test for sparrowNames found in parameters.csv but NOT in dataDictionary.csv and/or design_matrix.csv
            #terminate if missing found
            addVars(file.output.list, batch_mode)
            
            #create binary maps
            if (if_create_binary_maps=="yes"){
              setupMaps(file.output.list,mapping.input.list,batch_mode,RSPARROW_errorOption)
            }
            #create output directories
            dirCreated<-createDirs(file.output.list,if_userModifyData,
                                   batch_mode)
            
            
            #delete old files if_estimate or if_estimate_simulation
            if (if_estimate=="yes" | if_estimate_simulation=="yes"){
              deleteFiles(path_results)
            }
            
            ##############################################################
            if (batch_mode=="no"){    
              {cat("\n \n")
                run2<-ifelse(load_previousDataImport=="no",1,0)
                assign("run2",run2,envir = .GlobalEnv)
                cat("RSPARROW MODEL NAME: ",run_id,sep="")
                cat("\n \n")
                if (select_scenarioReachAreas=="yes"){
                  cat("SCENARIO NAME: ",scenario_name,sep="")
                  cat("\n \n")
                }
                cat("OUTPUT DIRECTORY: ",path_results,sep="")
                cat("\n \n")
                if (run2==1){
                  dataInputPrep(#for readData
                    file.output.list,input_data_fileName,
                    #for checkData1NavigationVars
                    if_reverse_hydseq,
                    #for createVerifyNavigationVars
                    if_verify_demtarea,calculate_reach_attribute_list,
                    mapping.input.list,
                    #for all
                    batch_mode)
                  
                }#if run2=yes
                if (load_previousDataImport=="yes"){
                  fileName<-strsplit(path_results,.Platform$file.sep)[[1]]
                  fileName<-paste(fileName[1:length(fileName)-1],collapse = .Platform$file.sep)
                  fileName<-paste0(fileName,.Platform$file.sep,gsub(".csv","",input_data_fileName),"_priorImport")
                  #check if file exists
                  if (file.exists(fileName)){
                    load(file=fileName)  
                  }else{
                    message(paste0("ERROR : ",fileName," binary file NOT FOUND\n SET load_previousDataImport<-'no'.\n RUN EXECUTION TERMINATED."))
                    errorOccurred("executeRSPARROW.R",batch_mode)
                  }
                  
                  
                }
              }#wait for run2 selection
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
              
              
              #remove unnecessary objects from workspace
              removeObjects(c("run2","saved","runScript","runRsparrow","dmatrixin","map_uncertainties"))
              
            }else{#batch run
              cat("\n \n")
              run2<-1
              assign("run2",run2,envir = .GlobalEnv)
              save(list = c(as.character(outputSettings(file.output.list,FALSE)$setting),
                            "runScript","run2","RSPARROW_errorOption",ls()[which(regexpr("path_",ls())>0)],
                            ls()[which(regexpr("file_",ls())>0)],
                            "estimate.input.list","mapping.input.list",
                            "file.output.list","class.input.list","min.sites.list","scenario.input.list",
                            "path_results","path_data","path_gis"),
                   file=paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData"))
              system(paste0(Sys.which("Rscript.exe")," ",file.path(paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchRun.R"))), wait = FALSE, invisible = FALSE)
              cat("Running RSPARROW in batch mode.")
              
              removeObjects(c("run2","saved","runScript","runRsparrow","dmatrixin","map_uncertainties"))
              
            }
          }#if runScript="yes"
          
        }#if no invalid settings
      }#if saved
    }#wait for saved selection 
    
  }#runOld
  
  
}#runNOw = yes




