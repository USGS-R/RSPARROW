#'@title startModelRun
#'@description executes sparrowSetup and sparrowExecution functions \\cr \\cr
#'Executed By: \\itemize\{\\item batchRun.R
#'             \\item executeRSPARROW.R\} \\cr
#'Executes Routines: \\itemize\{\\item applyUserModify.R
#'             \\item calcDemtareaClass.R
#'             \\item calcIncremLandUse.R
#'             \\item checkAnyMissingSubdataVars.R
#'             \\item checkClassificationVars.R
#'             \\item checkMissingSubdataVars.R
#'             \\item controlFileTasksModel.R
#'             \\item correlationMatrix.R
#'             \\item createDataMatrix.R
#'             \\item createMasterDataDictionary.R
#'             \\item createSubdataSorted.R
#'             \\item findMinMaxLatLon.R
#'             \\item modelCompare.R
#'             \\item named.list.R
#'             \\item outputSettings.R
#'             \\item readDesignMatrix.R
#'             \\item readParameters.R
#'             \\item selectCalibrationSites.R
#'             \\item selectDesignMatrix.R
#'             \\item selectParmValues.R
#'             \\item selectValidationSites.R
#'             \\item setNLLSWeights.R
#'             \\item shinyMap2.R
#'             \\item startEndmodifySubdata.R
#'             \\item unPackList.R\} \\cr
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param if_boot_estimate yes/no control setting to specify if parametric bootstrap estimation 
#'       (Monte Carlo) is to be executed
#'@param if_boot_predict yes/no control setting to specify if bootstrap predictions (mean, SE, 
#'       confidence intervals) are to be executed
#'@param enable_interactiveMaps yes/no control setting indicating whether shiny app should be 
#'       triggered at the end of the run
#'@param filter_data1_conditions User specified additional DATA1 variables (and conditions) to 
#'       be used to filter reaches from sparrow_control
#'@param data1 input data (data1)
#'@param if_userModifyData yes/no indicating whether or not the userModifyData.R control file 
#'       is to be applied
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param if_validate yes/no indicating whether or not validation is run
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param pvalidate numeric control setting indicating a percentage of calibration sites to 
#'       select for validation or if equal to 0 indicates that the user defined valsites variable should be 
#'       used to select sites for validation. For more details see documentation Section 4.4.6
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param biters User specified number of parametric bootstrap iterations from sparrow_control
#'@param compare_models character string control setting indicated the run_ids of preivously 
#'       run model to which the current model is to be compared
#'@param modelComparison_name character string control setting that gives the name of the 
#'       model comparision being executed
#'@param if_spatialAutoCorr yes/no control setting to specifiy if the spatial autocorrelation 
#'       diagnostic graphics for Moran's I test are to be output
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption 



startModelRun<-function(file.output.list,
                        if_estimate,if_estimate_simulation,
                        if_boot_estimate,if_boot_predict,enable_interactiveMaps,
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
                        compare_models,modelComparison_name,
                        if_spatialAutoCorr,
                        #shinyMap2
                        add_vars,
                        #batchAndError
                        batch_mode,
                        RSPARROW_errorOption){
  
  
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
  
  #compile master_dataDictionary
  createMasterDataDictionary(file.output.list,batch_mode)
  
  message("Reading parameters and design matrix...")
  # (A) input parameters and settings
  betavalues <- readParameters(file.output.list,if_estimate,if_estimate_simulation,
                               batch_mode)
  assign("betavalues",betavalues,envir = .GlobalEnv)
  
  # (B) Setup the parameter and system variables
  SelParmValues <- selectParmValues(betavalues,if_estimate,if_estimate_simulation,batch_mode)
  assign("SelParmValues",SelParmValues,envir = .GlobalEnv)
  #print("SELECTED PARAMETER VALUES")
  #SelParmValues 
  
  # deactivates unnecessary computation of parameter correlations, Eigenvalues in cases of one predictor variable
  if(SelParmValues$bcols == 1) {ifHess <- "no"
  assign("ifHess",ifHess,envir = .GlobalEnv)}  
  
  # (C) input source-delivery interaction matrix (includes all possible variables)
  dmatrixin <- readDesignMatrix(file.output.list,betavalues,batch_mode)
  assign("dmatrixin",dmatrixin,envir = .GlobalEnv)
  # (D) Setup design/interaction matrix
  dlvdsgn <- selectDesignMatrix(SelParmValues,betavalues,dmatrixin)
  assign("dlvdsgn",dlvdsgn,envir = .GlobalEnv)
  
  ##############################################################
  # 4. Create SUBDATA object for model estimation and prediction                         
  ##############################################################
  message("Creating and Modifying subdata...")
  # (A) Create 'subdata' by filtering DATA1
  subdata <- createSubdataSorted(filter_data1_conditions,data1)
  
  # (B) DATA MODIFICATIONS FUNCTION allows user-specified modification 
  #        of data and parameter variables in SUBDATA
  if (if_userModifyData=="yes"){
    
    tryIt<-try({ 
      
      subdata<-applyUserModify(file.output.list,
                               #modifySubdata arguments
                               betavalues,data_names,subdata,class_landuse,lon_limit)
    },TRUE)#end try
    
    if (class(tryIt)=="try-error"){#if an error occured
      cat("\n \n")
      message(paste("AN ERROR OCCURRED IN PROCESSING _userModifyData.R\n",
                    geterrmessage(),"RUN EXECUTION TERMINATED.",sep=""))
      if (batch_mode=="yes"){#if batch output message to log
        cat(" \nAN ERROR OCCURRED IN PROCESSING _userModifyData.R\n",
            geterrmessage(),"RUN EXECUTION TERMINATED.",sep="")
      }
      
      if (regexpr("not found",geterrmessage())>0){
        cat("\n \n")
        message(paste("OBJECT NOT FOUND ERROR COULD INDICATE THAT THE VARIABLE DOES NOT EXIST IN THE dataDictionary.csv FILE"))
        if (batch_mode=="yes"){#if batch output message to log
          cat(" \nOBJECT NOT FOUND ERROR COULD INDICATE THAT THE VARIABLE DOES NOT EXIST IN THE dataDictionary.csv FILE\n")
          
        } 
      }
      exit <- function() {
        .Internal(.invokeRestart(list(NULL, NULL), NULL))
      }
      exit() 
    }
    
    
    
  }else{
    subdata<-startEndmodifySubdata(data_names,class_landuse, data1)
  }
  assign("subdata",subdata,envir = .GlobalEnv)
  
  
  message("Testing for missing variables in subdata...")
  checkClassificationVars(subdata,class.input.list,batch_mode)
  checkMissingSubdataVars(subdata,betavalues,file.output.list,batch_mode)
  ###########################################################
  # 5. Setup calibration and validation sites and loads
  ###########################################################
  
  # (A) Calibration site filtering, based on 'calsite' index variable
  #     Filter the monitoring sites based on user-specified criteria (above)
  #     (Generate site count summary:  numsites1, numsites2, numsites3, numsites4, nMon)
  message("Setting up calibration and validation sites...")
  Csites.list <- selectCalibrationSites(subdata,data_names,min.sites.list)
  
  waterid <- Csites.list$waterid
  depvar <- Csites.list$depvar
  staid <- Csites.list$staid
  staidseq <- Csites.list$staidseq
  xx <- data.frame(waterid,staid,staidseq,depvar)
  drops <- c("depvar","staid","staidseq")
  subdata <- subdata[ , !(names(subdata) %in% drops)]
  subdata <- merge(subdata,xx,by="waterid",all.y=FALSE,all.x=FALSE)
  subdata <- subdata[with(subdata,order(subdata$hydseq)), ]     # resort by the original HYDSEQ order
  
  numsites1 <- Csites.list$numsites1 
  numsites2 <- Csites.list$numsites2
  numsites3 <- Csites.list$numsites3
  numsites4 <- Csites.list$numsites4
  nMon <- Csites.list$nMon
  nMoncalsites <- sum(ifelse(subdata$calsites==0 | is.na(subdata$calsites),0,1))
  
  # (B) Select validation sites 
  vic <- 0
  vsitedata <- NA
  Vsites.list <- NA
  if(if_validate == "yes") {
    Vsites.list <- selectValidationSites(iseed,pvalidate,subdata,minimum_reaches_separating_sites,data_names)
    
    waterid <- Vsites.list$waterid
    depvar <- Vsites.list$depvar
    staid <- Vsites.list$staid
    staidseq <- Vsites.list$staidseq
    vdepvar <- Vsites.list$vdepvar
    vstaid <- Vsites.list$vstaid
    vstaidseq <- Vsites.list$vstaidseq
    xx <- data.frame(waterid,staid,staidseq,depvar,vstaid,vstaidseq,vdepvar)
    drops <- c("depvar","staid","staidseq")
    subdata <- subdata[ , !(names(subdata) %in% drops)]
    subdata <- merge(subdata,xx,by="waterid",all.y=FALSE,all.x=FALSE)
    subdata <- subdata[with(subdata,order(subdata$hydseq)), ]     # resort by the original HYDSEQ order
    
    nMon <- Vsites.list$nMon
    nMoncalsites <- sum(ifelse(subdata$calsites==0 | is.na(subdata$calsites),0,1))
    vic <- Vsites.list$vic
    Csites.list$depvar <- Vsites.list$depvar
    Csites.list$staid <- Vsites.list$staid
    Csites.list$nMon <- Vsites.list$nMon
    Csites.list$nMoncalsites <- nMoncalsites
    Csites.list$staidseq <- Vsites.list$staidseq
    assign("Vsites.list",Vsites.list,envir = .GlobalEnv)
    
  }
  print(paste("Initial monitoring site count: ",numsites1,sep=""))
  
  print(paste("Monitoring sites after filtering for small headwater sites: ",numsites2,sep=""))
  
  print(paste("Monitoring sites after filtering for minimum number of reaches separating sites: ",numsites3,sep=""))
  
  print(paste("Monitoring sites after filtering for minimum incremental area between sites: ",numsites4,sep=""))
  
  print(paste("Number of calibration sites identified by the CALSITES variable: ",nMoncalsites,sep=""))
  
  print(paste("Number of selected calibration sites with non-zero observed loads: ",nMon,sep=""))
  
  print(paste("Number of selected validation sites with non-zero observed loads: ",vic,sep=""))
  
  save(subdata,file=paste(path_results,.Platform$file.sep,"data",.Platform$file.sep,"subdata",sep=""))
  
  ###############################################################
  # 6. Missing data checks and data setup for estimation 
  ###############################################################
  message("Setting up data for estimation...")
  # (A) Check for missing data in SUBDATA
  checkAnyMissingSubdataVars(subdata,betavalues,batch_mode)
  
  # (B) Setup 'Data' and 'beta' matrices and 'data.index.list' for optimization
  DataMatrix.list <- createDataMatrix(if_mean_adjust_delivery_vars,subdata,SelParmValues,betavalues)
  assign("DataMatrix.list",DataMatrix.list,envir = .GlobalEnv)
  # (B1) Setup 'Data' and 'beta' matrices and 'data.index.list' for optimization w/ scaled parameters
  # DataMatrixEstimate.list <- createDataMatrixEstimate(if_auto_scaling,if_mean_adjust_delivery_vars,subdata,
  #                                                    SelParmValues,betavalues,batch_mode)
  
  
  # (C) Setup SITEDATA and VSITEDATA for diagnostics
  sitedata <- subdata[(subdata$depvar > 0), ]  # create site attribute object
  assign("sitedata",sitedata,envir = .GlobalEnv)
  numsites <- length(sitedata$waterid)
  save(sitedata,file=paste(path_results,.Platform$file.sep,"data",.Platform$file.sep,"sitedata",sep=""))
  assign("numsites",numsites,envir = .GlobalEnv)
  
  
  # Find minimum and maximum Lat/Lon values for setting mapping projection limits
  sitegeolimits <- findMinMaxLatLon(sitedata,mapping.input.list)
  assign("sitegeolimits",sitegeolimits,envir = .GlobalEnv)
  
  
  cat(" \nMonitoring station latitude and longitude minimums and maximums = \n ",sep="")
  print(unlist(sitegeolimits))
  cat("\n \n")
  
  
  vnumsites <- 0
  if(if_validate == "yes") {
    vsitedata <- subdata[(subdata$vdepvar > 0), ]  # create site attribute object
    assign("vsitedata",vsitedata,envir = .GlobalEnv)
    vnumsites <- length(vsitedata$waterid)
    vnumsites 
    save(vsitedata,file=paste(path_results,.Platform$file.sep,"data",.Platform$file.sep,"vsitedata",sep=""))
  }
  
  # (D) Setup land use for incremental basins for diagnostics 
  #      (includes setup of 'classvar2' classes in 'sitedata' and 'vsitedata' objects for 
  #      plotting diagnostics for non-contiguous variables)
  if(numsites > 0) {
    sitedata.landuse <- calcIncremLandUse(subdata,class_landuse,subdata$staidseq,minimum_reaches_separating_sites)
    # Obtain total drainage area classification variable for calibration sites
    sitedata.demtarea.class <- calcDemtareaClass(sitedata$demtarea)
    assign("sitedata.landuse",sitedata.landuse,envir = .GlobalEnv)
    assign("sitedata.demtarea.class",sitedata.demtarea.class,envir = .GlobalEnv)
  }
  if(vnumsites > 0) {
    vsitedata.landuse <- calcIncremLandUse(subdata,class_landuse,subdata$vstaidseq,minimum_reaches_separating_sites)
    # Obtain total drainage area classification variable for validation sites
    vsitedata.demtarea.class <- calcDemtareaClass(vsitedata$demtarea)
    assign("vsitedata.landuse",vsitedata.landuse,envir = .GlobalEnv)
    assign("vsitedata.demtarea.class",vsitedata.demtarea.class,envir = .GlobalEnv)
  }
  
  # if no classvar/classvar[1] then use sitedata.demtarea.class 5.5.17
  if (is.na(classvar) |is.na(classvar[1])){
    sitedata$sitedata.demtarea.class<-sitedata.demtarea.class
    assign("sitedata",sitedata,envir = .GlobalEnv)
    if(vnumsites > 0) {
      vsitedata$sitedata.demtarea.class<-vsitedata.demtarea.class 
      assign("vsitedata",vsitedata,envir = .GlobalEnv)
    }
    
    
    
    
    classvar[1]<-"sitedata.demtarea.class"
    uniqueClasses<-unique(sitedata.demtarea.class)[order(unique(sitedata.demtarea.class))]
    demtarea.rchclass <- numeric(length(subdata$waterid))
    demtarea.rchclass <- rep(uniqueClasses[1],length(subdata$waterid))
    for (i in 1:length(subdata$waterid)) {
      for (k in 1:(length(uniqueClasses)-1)) {
        if(uniqueClasses[k] < subdata$demtarea[i] & subdata$demtarea[i] <= uniqueClasses[k+1]) {
          demtarea.rchclass[i] <- uniqueClasses[k+1]
        } 
      }
    }
    subdata$sitedata.demtarea.class<-demtarea.rchclass
    class.input.list$classvar<-classvar
    assign("class.input.list",class.input.list,envir = .GlobalEnv)
    assign("classvar",classvar,envir = .GlobalEnv)
    
  }
  
  
  # (E) Run correlations among explanatory variables
  #     NOTE: needs conversion to subroutine; HUC8 computation of mean to reduce size needs generalization
  Cor.ExplanVars.list <- NA
  if (if_corrExplanVars == "yes") {
    maxsamples <- 500
    # select user-specified names from 'parmCorrGroup' in the parameters.csv file
    names <- SelParmValues$sparrowNames[SelParmValues$bCorrGroup==1]
    if(length(names)>1) {   # minimum of 2 explanatory variables required
      message("Running correlations among explanatory variables...")
      Cor.ExplanVars.list <- correlationMatrix(file.output.list,SelParmValues,subdata)
      
      objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_Cor.ExplanVars.list",sep="")
      save(Cor.ExplanVars.list,file=objfile)
    }
  }
  
  ########################################
  ### 7. PERFORM MODEL EXECUTION TASKS ###
  ########################################
  
  # variables from prior data processing subroutines: numsites,data_names,sitedata.landuse,vsitedata.landuse,
  #        Csites.weights.list
  
  # Calculate regression weights as proportional to incremental area size
  weight<-NA
  Csites.weights.list <- named.list(weight)
  if(numsites > 0) {
    Csites.weights.list <- setNLLSWeights(NLLS_weights,run_id,subdata,sitedata,data_names,
                                          minimum_reaches_separating_sites,batch_mode)
    if(NLLS_weights == "default") {
      Csites.weights.list$weight <- rep(1,numsites) # set weights=1.0 (overide area-based weights)
    }
  }
  assign("Csites.weights.list",Csites.weights.list,envir = .GlobalEnv)
  assign("Csites.list",Csites.list,envir = .GlobalEnv)
  assign("Cor.ExplanVars.list",Cor.ExplanVars.list,,envir = .GlobalEnv)
  assign("subdata",subdata,envir = .GlobalEnv)
  
  
  runTimes <- controlFileTasksModel(
    # pathnames
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
    RSPARROW_errorOption)
  
  
  
  
  
  if(if_boot_estimate == "yes") {
    cat(" \nBootstrap estimation run time\n ")
    print(runTimes$BootEstRunTime)
  }
  
  if(if_boot_predict == "yes") {
    cat(" \nBootstrap prediction run time\n ")
    print(runTimes$BootPredictRunTime)
  }
  
  if(!is.na(master_map_list[1])) {
    cat(" \nMap predictions run time\n ")
    print(runTimes$MapPredictRunTime)
  }
  
  
  #output user settings
  allSettings<-outputSettings(file.output.list,TRUE)
  
  #run model comparison
  modelCompare(file.output.list,compare_models,modelComparison_name,if_spatialAutoCorr)
  
  #popup results
  try({
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      shell.exec(paste(path_results,"estimate",.Platform$file.sep,run_id,"_diagnostic_plots.pdf",sep=""))
      shell.exec(paste(path_results,"estimate",.Platform$file.sep,run_id,"_summary.txt",sep=""))
    }
  },silent=TRUE)
  
  
  # obtain uncertainties, if available
  objfile <- paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_BootUncertainties",sep="")
  if(file.exists(objfile) == TRUE) {
    load(objfile)
    map_uncertainties <- c("se_pload_total","ci_pload_total")
  } else {
    map_uncertainties <- NA 
    BootUncertainties <- NA
  }
  assign("map_uncertainties",map_uncertainties,envir = .GlobalEnv)
  assign("BootUncertainties",BootUncertainties,envir = .GlobalEnv)
  
  if (enable_interactiveMaps=="yes" & batch_mode=="no"){
    #setup for interactive Mapping
    shiny::runApp(shinyMap2(
      #stream/catchment
      file.output.list,map_uncertainties,BootUncertainties,
      data_names,mapping.input.list,
      #predict.list,
      subdata,SelParmValues,
      #site attr
      sitedata,
      #scenarios
      estimate.list,
      ConcFactor,DataMatrix.list,dlvdsgn,
      reach_decay_specification,reservoir_decay_specification,
      scenario.input.list,
      #scenarios out
      add_vars,
      #batchError
      batch_mode,
      RSPARROW_errorOption))
    stopApp()
    
    
  }#end interactive maps
  
  # }#if runScript=yes
  #}#if exists(runScript)
  
  
  
  
}#end function
