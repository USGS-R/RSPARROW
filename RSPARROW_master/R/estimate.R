#'@title estimate
#'@description Executes all model tasks related to model estimation, diagnostics plotting and 
#'            mapping, parameter sensitivities, validation diagnostics, and the output of tabular summary metrics. \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R
#'             \\item diagnosticSensitivity.R
#'             \\item estimateFeval.R
#'             \\item estimateFevalNoadj.R
#'             \\item estimateNLLSmetrics.R
#'             \\item estimateNLLStable.R
#'             \\item estimateOptimize.R
#'             \\item named.list.R
#'             \\item predict.R
#'             \\item predictSummaryOutCSV.R
#'             \\item unPackList.R
#'             \\item validateMetrics.R\} \\cr
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param class.input.list list of control settings related to classification variables
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches 
#'       separating sites
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & 
#'       ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param Csites.list list output from `selectCalibrationSites.R` modified in `startModelRun.R`
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param numsites number of sites selected for calibration
#'@param if_validate yes/no indicating whether or not validation is run
#'@param Vsites.list named list of sites for validation
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0
#'                 & subdata$calsites==1), ]`
#'@param subdata data.frame input data (subdata)
#'@param Cor.ExplanVars.list list output from `correlationMatrix.R`
#'@param sitedata.landuse Land use for incremental basins for diagnostics.
#'@param vsitedata.landuse Land use for incremental basins for diagnostics for validation 
#'                         sites.
#'@param sitedata.demtarea.class Total drainage area classification variable for calibration 
#'                               sites.
#'@param vsitedata.demtarea.class Total drainage area classification variable for validation 
#'                                sites.
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `estimate.list` named list of summary metrics and diagnostic output. For more 
#'            details see documentation section 5.2.4



estimate <- function(if_estimate,if_predict,file.output.list,
                     class.input.list,dlvdsgn,
                     estimate.input.list,
                     minimum_reaches_separating_sites,
                     DataMatrix.list,SelParmValues,Csites.weights.list,Csites.list,sitedata,numsites,
                     if_validate,Vsites.list,vsitedata,subdata,
                     Cor.ExplanVars.list,
                     sitedata.landuse,vsitedata.landuse,sitedata.demtarea.class,vsitedata.demtarea.class,
                     mapping.input.list,betavalues,
                     if_estimate_simulation,
                     batch_mode) {
  
  
  #########################################
  
  # create global variable from list names
  unPackList(lists = list(estimate.input.list = estimate.input.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,NA,NA))
  
  estimate.list <- NULL
  
  
  
  if (if_estimate == "yes" & if_estimate_simulation == "no") {
    message("Running estimation...")
    sparrowEsts <- estimateOptimize(file.output.list,SelParmValues,estimate.input.list,
                                    DataMatrix.list,dlvdsgn)
    
    #   Resids <- sparrowEsts$resid / sqrt(Csites.weights.list$weight)  # re-express residuals in original units
    # Calculate estimation diagnostic metrics (JacobResults, HesResults)
    if_sparrowEsts <- 1  # calculation of Jacobian diagnostics
    estimate.metrics.list <- estimateNLLSmetrics(if_estimate,if_estimate_simulation,if_sparrowEsts,sparrowEsts,
                                                 file.output.list,classvar,dlvdsgn,
                                                 Csites.weights.list,estimate.input.list,
                                                 Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,batch_mode)
    
    # unpack estimate.metrics.list for return
    JacobResults <- estimate.metrics.list$JacobResults
    HesResults <- estimate.metrics.list$HesResults
    ANOVA.list <- estimate.metrics.list$ANOVA.list
    Mdiagnostics.list <- estimate.metrics.list$Mdiagnostics.list
    estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)
    
    
    if(if_validate == "yes") {
      message("Running Validation...")
      validate.metrics.list <- validateMetrics(classvar,estimate.list,dlvdsgn,Vsites.list,yieldFactor,
                                               SelParmValues,subdata,vsitedata,DataMatrix.list)
      vANOVA.list <- validate.metrics.list$vANOVA.list
      vMdiagnostics.list <- validate.metrics.list$vMdiagnostics.list
      estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list,
                                  vANOVA.list,vMdiagnostics.list)
    }
    
    
    # Output summary metrics 
    estimateNLLStable(file.output.list,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                      classvar,sitedata,numsites,
                      ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                      Cor.ExplanVars.list,
                      if_validate,vANOVA.list,vMdiagnostics.list,betavalues,Csites.weights.list)
    
    
    ####################################################
    ### 1. Output diagnostic graphics (plots & maps) ###
    message("Running diagnostic plots and sensitivity analysis...")
    if(is.finite(JacobResults$mean_exp_weighted_error)){   # mean error and standardized residuals must be available
      diagnosticPlotsNLLS(file.output.list,class.input.list,sitedata.demtarea.class,
                          sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,Cor.ExplanVars.list,
                          data_names,add_vars,batch_mode)
    }
    ##############################################
    ### 3. Sensitivity analyses for parameters ###
    
    diagnosticSensitivity(file.output.list,classvar,estimate.list,DataMatrix.list,SelParmValues,
                          reach_decay_specification,reservoir_decay_specification,
                          subdata,sitedata.demtarea.class,mapping.input.list)
    
    #####################################
    ### 4. Output validation metrics  ###
    
    if(if_validate == "yes") {
      diagnosticPlotsValidate(file.output.list,class.input.list,vsitedata.demtarea.class,
                              vsitedata,vsitedata.landuse,estimate.list,mapping.input.list,add_vars,batch_mode)
      
    } # end validate loop
    ##########################################
  } else { 
    
    # No estimation desired; load previous results if exist, including Jacobian to support prediction
    #   Files checked include:  sparrowEsts, HessianResults, JacobResults
    #   These files are needed for prediction and bootstrap prediction
    
    objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_sparrowEsts")
    if(file.exists(objfile) == TRUE & if_estimate_simulation=="no") {
      load(objfile)
      # Load SPARROW object file from a prior run if available
      # Contents of "sparrowEsts" object:  
      # $resid - station model residuals
      # $jacobian - jacobian results
      # $coefficients - estimated NLLS values
      
      # $ssquares - total sum of square of error for the NLLS 
      # $lower - lower parameter bounds for the least squares estimation
      # $upper - upper parameter bounds for the least squares estimation
      
      
      objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults")
      if(file.exists(objfile) == TRUE) {
        load(objfile)
      }
      
      estimate.list <- named.list(sparrowEsts,JacobResults)
      assign("estimate.list",estimate.list, envir=.GlobalEnv)
      
      # load the Hessian results if object exists
      objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_HessianResults")
      if(file.exists(objfile) == TRUE) {
        load(objfile)
        estimate.list <- named.list(sparrowEsts,JacobResults,HesResults)
        assign("estimate.list",estimate.list, envir=.GlobalEnv)
      }
      
      ##########################################
    } else {       # no prior estimates, thus use starting values to make predictions
      #  and create 'sparrowEsts' and 'JacobResults' objects for return
      
      if (if_estimate_simulation == "yes") {                  
        message("Running model in simulation mode using initial values of the parameters...")
        sparrowEsts<-alist(SelParmValues=)$beta0
        sparrowEsts$coefficient <- SelParmValues$beta0   # starting values
        
        
        nn <- ifelse(DataMatrix.list$data[,10] > 0 # jdepvar site load index
                     & DataMatrix.list$data[,13]==1, #calistes ==1
                     1,0)  
        
        # if monitoring loads exist (but not estimating coefs), run residuals, performance measures, 
        #     and save JacobResults objects 
        
        if(sum(nn) > 0) {     
          
          message("Outputing performance diagnostics for simulation mode...")
          
          # compute Resids
          sparrowEsts$resid <- estimateFevalNoadj(SelParmValues$beta0,
                                                  DataMatrix.list,SelParmValues,Csites.weights.list,
                                                  estimate.input.list,dlvdsgn)  # estimate using starting values
          sparrowEsts$coefficients <- SelParmValues$beta0
          
          # OUTPUT ESTIMATION METRICS & SUMMARY TABLE
          if_sparrowEsts <- 0   # no calculation of Jacobian diagnostics
          estimate.metrics.list <- estimateNLLSmetrics(if_estimate,if_estimate_simulation,if_sparrowEsts,sparrowEsts,
                                                       file.output.list,classvar,dlvdsgn,
                                                       Csites.weights.list,estimate.input.list,
                                                       Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,batch_mode)
          
          # unpack estimate.metrics.list for return
          JacobResults <- estimate.metrics.list$JacobResults
          HesResults <- estimate.metrics.list$HesResults
          ANOVA.list <- estimate.metrics.list$ANOVA.list
          Mdiagnostics.list <- estimate.metrics.list$Mdiagnostics.list
          estimate.list <- named.list(sparrowEsts,JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)
          
          # save sparrowEsts file to support prediction
          objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_sparrowEsts")
          save(sparrowEsts,file=objfile)
          
          
          estimateNLLStable(file.output.list,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                            classvar,sitedata,numsites,
                            ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                            Cor.ExplanVars.list,
                            if_validate,vANOVA.list,vMdiagnostics.list,betavalues,Csites.weights.list)
          
          
          ### Output diagnostic graphics (plots & maps) ###
          message("Running diagnostic plots and sensitivity analysis...")
          diagnosticPlotsNLLS(file.output.list,class.input.list,sitedata.demtarea.class,
                              sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,Cor.ExplanVars.list,
                              data_names,add_vars,batch_mode)
          
          ### Sensitivity analyses for parameters ###
          diagnosticSensitivity(file.output.list,classvar,estimate.list,DataMatrix.list,SelParmValues,
                                reach_decay_specification,reservoir_decay_specification,
                                subdata,sitedata.demtarea.class,mapping.input.list)
          
          
        } else {
          
          # if no monitoring loads, store Jacobian estimates in object as list for use in making predictions only
          JacobResults<-alist(JacobResults=)$SelParmValues$beta0   # starting values 
          JacobResults$oEstimate <- SelParmValues$beta0
          JacobResults$Parmnames <- noquote(c(SelParmValues$srcvar,SelParmValues$dlvvar,
                                              SelParmValues$decvar,SelParmValues$resvar))
          
          
          objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults")
          save(JacobResults,file=objfile)
          
          objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_sparrowEsts")
          save(sparrowEsts,file=objfile)
          
          estimate.list <- named.list(sparrowEsts,JacobResults)
        }
        
        
      } # end if_estimate_simulation check
      
    } # end else for 'no prior estimates exist' case
    
  }  # end "if_estimate" check
  
  
  ##########################################
  # Compute summary statistics for predictions
  
  if(if_predict == "yes"  & if_estimate == "yes") {
    
    # Calculate and output standard bias-corrected predictions
    #  Note:  these include adjusted and nonadjusted for monitoring loads
    if(is.null(estimate.list$JacobResults$mean_exp_weighted_error) == TRUE) {
      bootcorrection <- 1.0
    } else {
      bootcorrection <- estimate.list$JacobResults$mean_exp_weighted_error
    }
    
    if(is.finite(bootcorrection)) {
      message("Running summary predictions...")
      predict.list <- predict(estimate.list,estimate.input.list,bootcorrection,DataMatrix.list,
                              SelParmValues,subdata)
      
      predictSummaryOutCSV(file.output.list,estimate.input.list,
                           SelParmValues,estimate.list,predict.list,
                           subdata,class.input.list)
    } else {
      message("Summary predictions not executed; mean_exp_weighted_error and predictions = infinity; 
               check standardized residuals for outliers...")
    }
    
  }
  
  ##########################################
  if (if_estimate=="yes"|if_estimate_simulation=="yes"){      
    for (l in names(estimate.list)){
      assign(l,get(l),envir=.GlobalEnv)   
      if (l=="Mdiagnostics.list"){
        # store Mdiagnostics.list and ANOVA.list objects as list
        objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_Mdiagnostics.list")
        save(list=l,file=objfile)
        objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_ANOVA.list")
        save(ANOVA.list,file=objfile)
        
        if(if_validate == "yes" & if_estimate_simulation=="no") {
          # store vMdiagnostics.list and vANOVA.list objects as list
          objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_vMdiagnostics.list")
          save(vMdiagnostics.list,file=objfile)
          objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_vANOVA.list")
          save(vANOVA.list,file=objfile)
        }
      }
    }
    assign("estimate.list",estimate.list, envir=.GlobalEnv)
    
    
  }
  
  
  return(estimate.list)
}#end function  # return

