#'@title controlFileTasksModel
#'@description Executes all model tasks after checks have been performed, including execution 
#'            of model estimation, diagnostics, prediction, scenarios, and mapping \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item batchMaps.R
#'             \\item diagnosticSpatialAutoCorr.R
#'             \\item estimate.R
#'             \\item estimateBootstraps.R
#'             \\item named.list.R
#'             \\item outputSettings.R
#'             \\item predict.R
#'             \\item predictBootsOutCSV.R
#'             \\item predictBootstraps.R
#'             \\item predictOutCSV.R
#'             \\item predictScenarios.R
#'             \\item unPackList.R\} \\cr
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param subdata data.frame input data (subdata)
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0), ]`
#'@param numsites number of sites selected for calibration
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches 
#'       separating sites
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param if_validate yes/no indicating whether or not validation is run
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param biters User specified number of parametric bootstrap iterations from sparrow_control
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption 
#'@return `runTimes`  named list of run times for bootstrap estimation, bootstrap prediction, 
#'            mapping, and estimate.list



controlFileTasksModel <- function(# pathnames
  file.output.list,
  # parameters
  SelParmValues,betavalues,
  # NLR weights
  Csites.weights.list,
  # data
  subdata,data_names,DataMatrix.list,sitedata,Vsites.list,vsitedata,numsites,
  # land use classifcation 
  class.input.list,
  # explanatory variable correlations
  Cor.ExplanVars.list,
  # estimation
  minimum_reaches_separating_sites,
  if_estimate,if_estimate_simulation,dlvdsgn,
  estimate.input.list,
  #prediction
  if_predict,
  #diagnostics and validation
  if_validate,sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
  #mapping 
  mapping.input.list,
  #bootstrapping
  iseed,biters,
  #scenarios
  scenario.input.list,
  #batch_mode
  batch_mode,
  RSPARROW_errorOption){
  
  
  #####################################
  ### PERFORM MODEL EXECUTION TASKS ###
  #####################################
  
  #######################
  ### A. OPTIMIZATION ###
  #######################
  
  # Estimation options for if_estimate=""
  # (1) if_estimate="yes" - executes NLLS; calculates estimation metrics, and outputs ANOVA summary table
  # (2) if_estimate="no" and prior NLLS execution completed (sparrowEsts object) - calculates estimation,
  #      metrics, and outputs ANOVA summary table (Hessian output if previously executed)
  # (3) if_estimate="no" stores starting beta values in JacobResults for use in making predictions
  #      if monitoring loads exist, then calculates estimation metrics, and outputs ANOVA summary table
  # (4) if_predict="yes" - executes reach summary deciles; output to (prefix name)_summary_predictions.csv
  
  
  
  
  unPackList(lists = list(file.output.list = file.output.list,
                          estimate.input.list = estimate.input.list,
                          class.input.list = class.input.list,
                          scenario.input.list = scenario.input.list),
             parentObj = list(NA,NA,NA,NA))
  
  # creates or checks for prior estimation objects:  sparrowEsts,JacobResults,HesResults
  estimate.list <- estimate(if_estimate,if_predict,file.output.list,
                            class.input.list,dlvdsgn,
                            estimate.input.list,
                            minimum_reaches_separating_sites,
                            DataMatrix.list,SelParmValues,Csites.weights.list,Csites.list,sitedata,numsites,
                            if_validate,Vsites.list,vsitedata,subdata,
                            Cor.ExplanVars.list,
                            sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
                            mapping.input.list,betavalues,
                            if_estimate_simulation,
                            batch_mode)
  
  # save the DataMatrix.list used to estimate the model
  objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_DataMatrix.list",sep="")
  save(DataMatrix.list,file=objfile)
  
  # save the SelParmValues information used to estimate the model
  objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_SelParmValues",sep="")
  save(SelParmValues,file=objfile)
  
  #  HesRunTime <- estimate.list$HesResults$HesRunTime
  
  ######################
  ### B. DIAGNOSTICS ###
  ######################
  
  if(if_spatialAutoCorr == "yes") {   
    
    nn <- ifelse(DataMatrix.list$data[,10] > 0,1,0)  # jdepvar site load index (monitoring sites must be present)
    if( (if_estimate=="yes" | if_estimate_simulation=="yes") & (sum(nn) > 0) ) {
      
      ############################################
      ### 2. Spatial auto-correlation analysis ###
      ############################################
      # Moran's I analysis (requires execution of diagnosticPlotsNLLS)
      message("Running diagnostic spatial autocorrelation...")
      # only execute if stream 'length' available for all reaches
      if(sum(!is.na(subdata$length))>0) {
        diagnosticSpatialAutoCorr(file.output.list,classvar,sitedata,numsites,estimate.list,
                                  estimate.input.list,subdata)
      }
      
    } else {
      message("Diagnostic spatial autocorrelation was not executed; 
      execution requires if_estimate or if_estimate_simulation = yes and monitoring site loads must be available.")
    }
    
  } # end if_spatialAutoCorr for production of graphics
  
  
  #####################################
  ### C. BOOTSTRAP MODEL ESTIMATION ###
  #####################################
  
  # Run parametric bootstrap option for parameter estimation (if_boot_estimate = "yes")
  # Obtain previous results if exist (if_boot_estimate = "no")
  BootEstRunTime <- " "
  ptm <- proc.time()
  
  objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_HessianResults",sep="") # needed for covariance 
  
  if(if_boot_estimate == "yes" & file.exists(objfile) == TRUE) {
    message("Estimating bootstrap model coefficients and errors...")
    
    BootResults <- estimateBootstraps(iseed,biters,estimate.list,
                                      DataMatrix.list,SelParmValues,Csites.weights.list,
                                      estimate.input.list,dlvdsgn,file.output.list)
    
  } else {  # no bootstrap estimation; check to see if already exists
    
    objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_BootBetaest",sep="")  # BootResults
    if(file.exists(objfile) == TRUE) {
      load(objfile)
    } else {
      objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_HessianResults",sep="") # needed for covariance 
      if(if_boot_estimate == "yes" & file.exists(objfile) == FALSE) {
        message(paste(" \nWARNING : HesResults DOES NOT EXIST.  boot_estimate NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
        if (batch_mode=="yes"){
          cat(paste(" \nWARNING : HesResults DOES NOT EXIST.  boot_estimate NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
        } 
      }
    }
    
  } # end check whether bootstrap file exist
  BootEstRunTime <- proc.time() - ptm
  # 250 secs for 10 iterations for bootstrap estimation
  
  
  #############################
  ### D. OUTPUT PREDICTIONS ###
  #############################
  
  # Load predictions:
  #   pload_total                Total load (fully decayed)
  #   pload_(sources)            Source load (fully decayed)
  #   mpload_total               Monitoring-adjusted total load (fully decayed)
  #   mpload_(sources)           Monitoring-adjusted source load (fully decayed)
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_nd_(sources)         Source load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_(sources)        Source incremental load delivered to streams
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  #   pload_inc_deliv            Total incremental load delivered to terminal reach
  #   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
  #   share_total_(sources)      Source shares for total load (percent)
  #   share_inc_(sources)        Source share for incremental load (percent)
  
  # Yield predictions:
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   yield_(sources)            Source yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_(sources)        Source incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach
  #   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach
  
  # Uncertainty predictions (requires prior execution of bootstrap predictions):
  #   se_pload_total             Standard error of the total load (percent of mean)
  #   ci_pload_total             95% prediction interval of the total load (percent of mean)
  
  #############################################
  #### 1. Standard Predictions to CSV file ####
  #############################################
  
  BootPredictRunTime <- " "
  MapPredictRunTime <- " "
  
  if(if_predict == "yes") {
    
    objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults",sep="")
    if(file.exists(objfile) == TRUE) {
      
      message("Running predictions...")     
      
      # Calculate and output standard bias-corrected predictions
      #  Note:  these include adjusted and nonadjusted for monitoring loads
      if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
        bootcorrection <- 1.0
      } else {
        bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
      }
      
      predict.list <- predict(estimate.list,estimate.input.list,bootcorrection,DataMatrix.list,
                              SelParmValues,subdata)
      
      objfile <- paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep="")
      save(predict.list,file=objfile)
      predictOutCSV(file.output.list,estimate.list,predict.list,subdata,
                    add_vars,data_names)
      
    }  # end check on JacobResults exist
  } # end if_predict    
  
  ###############################################
  #### 2. Bootstrap Prediction to CSV file ######
  ###############################################
  
  # Requires prior execution of bootstrap estimation and standard predictions
  ptm <- proc.time()
  if(if_boot_predict == "yes") { 
    
    if (file.exists(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_BootBetaest",sep="")) &
        (file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep="")) | if_predict == "yes")  )  {
      
      load(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_BootBetaest",sep=""))
      load(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))
      
      assign("BootResults",BootResults,envir = .GlobalEnv)
      
      message("Running bootstrap predictions...")
      predictBoots.list <- predictBootstraps(iseed,biters,estimate.list,estimate.input.list,predict.list,BootResults,
                                             DataMatrix.list,SelParmValues,
                                             subdata,file.output.list)
      
      objfile <- paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predictBoots.list",sep="")
      save(predictBoots.list,file=objfile)
      
      predictBootsOutCSV(file.output.list,estimate.list,predictBoots.list,subdata,
                         add_vars,data_names)   # edit preditBootsOutCSV accordingly
      
      
    } else if (!file.exists(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_BootBetaest",sep=""))){
      message(paste(" \nWARNING : BootBetaest DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
      if (batch_mode=="yes"){
        cat(paste(" \nWARNING : BootBetaest DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
      }   
    }else if (!(file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep="")) | if_predict == "yes")){
      message(paste(" \nWARNING : predict.list DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
      if (batch_mode=="yes"){
        cat(paste(" \nWARNING : predict.list DOES NOT EXIST.  boot_predict NOT RUN.\n ",sep=""))     # please edit this with additional info to report on prediction condition as you see fit
      }   
    }#end if file.exists
    
  }  # end _BootBetaest check
  BootPredictRunTime <- proc.time() - ptm
  
  
  ########################################
  #### 3. Predictions Mapping Options ####
  ########################################
  
  ptm <- proc.time()
  if(!is.na(master_map_list[1])) {
    
    
    
    #predictMaps
    if (!is.na(output_map_type)){
      
      dir.create(paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",sep=""))
      mapScenarios<-FALSE
      Rshiny<-FALSE
      allMetrics<-NA
      scenarioFlag<-NA
      save(list = c(as.character(outputSettings(file.output.list,FALSE)$setting),
                    "runScript","run2","file.output.list","scenario.input.list","RSPARROW_errorOption",
                    ls()[(regexpr("file_",ls())>0)],"estimate.input.list","mapping.input.list","mapScenarios","Rshiny","allMetrics","scenarioFlag"),
           file=paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
      
      
      save(list=c("data_names"),
           file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""),compress = FALSE)
      
      message("Running Prediction Maps in batch mode.")
      system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchMaps.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
      
      
      unlink(paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",sep=""),recursive=TRUE)
      
      
    }
    
    
    
    
  }
  MapPredictRunTime <- proc.time() - ptm
  
  #######################################################
  #### 4. Prediction Load-Reduction Scenario Options ####
  #######################################################
  
  # NOTE: standard predictions ("if_predict = yes") must be executed to support this feature
  #       to ensure creation of load and yield matrices
  
  
  input<-list(variable="",scLoadCheck="",batch="",scYieldCheck="",domain="",selectReaches="",sourcesCheck="",factors="")

  if (exists("estimate.list") & !is.null(estimate.list)){
  predictScenarios(#Rshiny
    input,NA, output_map_type,FALSE,
    #regular
    estimate.input.list,
    predict.list,scenario.input.list,
    data_names,estimate.list$JacobResults,if_predict,
    #bootcorrection,
    DataMatrix.list,SelParmValues,subdata,
    #predictStreamMapScenarios
    file.output.list,
    #scenarios out
    add_vars,
    mapping.input.list,
    batch_mode,
    RSPARROW_errorOption)
  }
  
  ##########################################
  
  runTimes <- named.list(BootEstRunTime,BootPredictRunTime,MapPredictRunTime,estimate.list) 
  
  
  
  
  return(runTimes)
  
}#end function

